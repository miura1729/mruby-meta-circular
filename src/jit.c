/*
** jit.c - Toplevel of JIT
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/opcode.h>
#include <mruby/error.h>
#include "value_array.h"
#include "mruby/jit.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "mruby/proc.h"
#include "mruby/class.h"
#include "mruby/array.h"
#include "mruby/throw.h"
#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>

#ifdef JIT_DEBUG
# define DEBUG(x) (x)
#else
# define DEBUG(x)
#endif

#define CI_ACC_SKIP    -1
#define CI_ACC_DIRECT  -2
#define CI_ACC_RESUMED -3

#define ARENA_RESTORE(mrb,ai) (mrb)->gc.arena_idx = (*status->ai)

int mrb_patch_irep_var2fix(mrb_state *, mrb_irep *, mrb_int);

int
mrbjit_check_inlineble(mrb_state *mrb, mrb_irep *irep)
{
  int i;

  if (irep->ilen >= NO_INLINE_METHOD_LEN || 1) {
    return 0;
  }

  for (i = 0; i < irep->ilen; i++) {
    switch(GET_OPCODE(irep->iseq[i])) {
    case OP_NOP:
    case OP_MOVE:
    case OP_LOADL:
    case OP_LOADI:
    case OP_LOADSYM:
    case OP_LOADSELF:
    case OP_LOADNIL:
    case OP_LOADT:
    case OP_LOADF:
    case OP_GETGLOBAL:
    case OP_SETGLOBAL:
    case OP_GETIV:
    case OP_SETIV:
    case OP_GETCV:
    case OP_SETCV:
    case OP_GETCONST:
    case OP_GETMCNST:
    case OP_SENDB:
    case OP_SEND:
    case OP_CALL:
    case OP_ENTER:
    case OP_RETURN:
    case OP_ADD:
    case OP_SUB:
    case OP_MUL:
    case OP_DIV:
    case OP_ADDI:
    case OP_SUBI:
    case OP_EQ:
    case OP_LT:
    case OP_LE:
    case OP_GT:
    case OP_GE:
    case OP_ARRAY:
    case OP_ARYCAT:
      //case OP_GETUPVAR:
      //case OP_SETUPVAR:
    case OP_JMP:
    case OP_JMPIF:
    case OP_JMPNOT:
      //case OP_LAMBDA:
    case OP_RANGE:
    case OP_STRING:
    case OP_STRCAT:
    case OP_HASH:
      //case OP_BLKPUSH:
      break;

    default:
      return 0;
    }
  }

  return 1;
}

void *
mrbjit_exec_send_c(mrb_state *mrb, mrbjit_vmstatus *status,
		 mrb_method_t m, struct RClass *c)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  mrb_irep *irep = *status->irep;
  mrb_sym *syms = irep->syms;
  int ai = *status->ai;
  mrb_code i = *pc;

  int a = GETARG_A(i);
  int n = GETARG_C(i);
  mrb_callinfo *ci;
  mrb_value recv, result;
  mrb_sym mid = syms[GETARG_B(i)];
  int orgdisflg = mrb->compile_info.disable_jit;
  int bidx;
  mrb_value blk;

  /* from OP_ADDI, OP_SUBI */
  switch (GET_OPCODE(i)) {
  case OP_ADDI:
  case OP_SUBI:
    n = 1;
  }

  recv = regs[a];
  if (n == CALL_MAXARGS) {
    bidx = a+2;
  }
  else {
    bidx = a+n+1;
  }
  blk = regs[bidx];

  // printf("C %d %x %x %x\n", m->body.func, regs, n);
  //puts(mrb_sym2name(mrb, mid));

  if (MRB_DMETHOD_CFUNC_P(m)) {
    regs[a] = MRB_DMETHOD_CFUNC(m)(mrb, regs, n);

    mrb_gc_arena_restore(mrb, ai);
    if (mrb->exc) return status->gototable[0]; /* L_RAISE */
  }
  ci = mrbjit_cipush(mrb);
  ci->stackent = mrb->c->stack;
  if (n == CALL_MAXARGS) {
    ci->argc = -1;
    ci->nregs = 3;
  }
  else {
    ci->argc = n;
    ci->nregs = n + 2;
  }

  if (MRB_METHOD_PROC_P(m)) {
    ci->proc = MRB_METHOD_PROC(m);
  }
  else {
    ci->proc = NULL;
  }
  ci->target_class = c;

  ci->pc = pc + 1;
  ci->acc = a;

  /* prepare stack */
  mrb->c->stack += a;

  //mrb_p(mrb, recv);
  mrb->compile_info.disable_jit = 1;
  mrb->vmstatus = (void *)status;
  if (MRB_METHOD_PROC_P(m)) {
    result = MRB_METHOD_PROC(m)->body.func(mrb, recv);
  }
  else {
    result = MRB_METHOD_CFUNC(m)(mrb, recv);
  }
  mrb->compile_info.disable_jit = orgdisflg;
  mrb_gc_arena_restore(mrb, ai);
  //mrb_gc_arena_shrink(mrb, ai);

  if (mrb->exc) return status->gototable[0]; /* L_RAISE */
  ci = mrb->c->ci;
  if (GET_OPCODE(i) == OP_SENDB) {
    if (mrb_type(blk) == MRB_TT_PROC) {
      struct RProc *p = mrb_proc_ptr(blk);
      if (p && !MRB_PROC_STRICT_P(p) && MRB_PROC_ENV(p) == ci[-1].env) {
	p->flags |= MRB_PROC_ORPHAN;
      }
    }
  }
  if (ci != mrb->c->ci) {
    /* OP_SEND like method ex. __send__  */
    ci[-1].jit_entry = NULL;
  }
  if (!ci->target_class) { /* return from context modifying method (resume/yield) */
    if (ci->acc == CI_ACC_RESUMED) {
      mrb->jmp = *status->prev_jmp;
      regs[(*status->irep)->nlocals] = result;
      return status->gototable[5]; /* goto L_HALT */
    }
    else {
      irep = *(status->irep) = ci[-1].proc->body.irep;
      *(status->proc) = ci[-1].proc;
      *(status->pool) = irep->pool;
      *(status->syms) = irep->syms;
      mrb->c->stack[0] = result;
      mrb->c->stack = mrb->c->ci->stackent;
      *(status->pc) = ci->pc;
      mrbjit_cipop(mrb);

      return status->gototable[6]; /* goto L_DISPATCH */
    }
  }

  mrb->c->stack[0] = result;
  mrb->c->stack = mrb->c->ci->stackent;
  *(status->pc) = ci->pc;
  mrbjit_cipop(mrb);

  return NULL;
}

void *
mrbjit_exec_send_c_void(mrb_state *mrb, mrbjit_vmstatus *status,
		 mrb_method_t m, struct RClass *c)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  mrb_irep *irep = *status->irep;
  mrb_sym *syms = irep->syms;
  int ai = *status->ai;
  mrb_code i = *pc;

  int a = GETARG_A(i);
  int n = GETARG_C(i);
  mrb_callinfo *ci;
  mrb_value recv, result;
  mrb_sym mid = syms[GETARG_B(i)];
  int orgdisflg = mrb->compile_info.disable_jit;

  recv = regs[a];

  // printf("C %d %x %x %x\n", m->body.func, regs, n);
  // puts(mrb_sym2name(mrb, mid));

  ci = mrbjit_cipush(mrb);
  ci->stackent = mrb->c->stack;
  if (n == CALL_MAXARGS) {
    ci->argc = -1;
    ci->nregs = 3;
  }
  else {
    ci->argc = n;
    ci->nregs = n + 2;
  }

  if (MRB_METHOD_PROC_P(m)) {
    ci->proc = MRB_METHOD_PROC(m);
  }
  else {
    ci->proc = NULL;
  }
  ci->target_class = c;

  ci->pc = pc + 1;
  ci->acc = a;

  /* prepare stack */
  mrb->c->stack += a;

  //mrb_p(mrb, recv);
  mrb->compile_info.disable_jit = 1;
  if (MRB_METHOD_PROC_P(m)) {
    result = MRB_METHOD_PROC(m)->body.func(mrb, recv);
  }
  else {
    result = MRB_METHOD_CFUNC(m)(mrb, recv);
  }
  mrb->compile_info.disable_jit = orgdisflg;
  mrb_gc_arena_restore(mrb, ai);
  //mrb_gc_arena_shrink(mrb, ai);

  if (mrb->exc) return status->gototable[0]; /* L_RAISE */
  ci = mrb->c->ci;
  if (ci != mrb->c->ci) {
    /* OP_SEND like method ex. __send__  */
    ci[-1].jit_entry = NULL;
  }
  if (!ci->target_class) { /* return from context modifying method (resume/yield) */
    if (ci->acc == CI_ACC_RESUMED) {
      mrb->jmp = *status->prev_jmp;
      regs[(*status->irep)->nlocals] = result;
      return status->gototable[5]; /* goto L_HALT */
    }
    else {
      irep = *(status->irep) = ci[-1].proc->body.irep;
      *(status->proc) = ci[-1].proc;
      *(status->pool) = irep->pool;
      *(status->syms) = irep->syms;
      mrb->c->stack = mrb->c->ci->stackent;
      *(status->pc) = ci->pc;
      mrbjit_cipop(mrb);

      return status->gototable[6]; /* goto L_DISPATCH */
    }
    mrb->c->stack = mrb->c->ci->stackent;
    *(status->pc) = ci->pc;
    mrbjit_cipop(mrb);

    return status->optable[GET_OPCODE(**(status->pc))];
  }
  mrb->c->stack = mrb->c->ci->stackent;
  mrbjit_cipop(mrb);

  return NULL;
}

void *
mrbjit_exec_extend_callinfo(mrb_state *mrb, struct mrb_context *cxt, int size)
{
  mrb_callinfo *sci;
  mrb_callinfo *dci;

  cxt->cibase_org = (mrb_callinfo *)mrb_malloc(mrb, sizeof(mrb_callinfo)*size*2 + 64);
  sci = cxt->cibase;
  cxt->cibase = (mrb_callinfo *)(((uintptr_t)(cxt->cibase_org) + 63) & (~(64 - 1)));
  for (dci = cxt->cibase; sci <= cxt->ci; sci++, dci++) {
    *dci = *sci;
  }

  cxt->ci = cxt->cibase + size;
  cxt->ciend = cxt->cibase + size * 2;

  return NULL;
}

void *
mrbjit_exec_send_mruby(mrb_state *mrb, mrbjit_vmstatus *status,
		 struct RProc *m, struct RClass *c)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_irep *irep = *status->irep;
  mrb_sym *syms = irep->syms;
  mrb_code i = *pc;

  int a = GETARG_A(i);
  int n = GETARG_C(i);
  struct mrb_context *cxt = mrb->c;
  mrb_callinfo *ci = cxt->ci;
  mrb_sym mid = syms[GETARG_B(i)];

  /* push callinfo */
  int ridx = ci->ridx;

  if (ci + 1 == cxt->ciend) {
    size_t size = ci - cxt->cibase;

    cxt->cibase_org = (mrb_callinfo *)mrb_realloc(mrb, cxt->cibase_org, sizeof(mrb_callinfo)*size*2 + 64);
    cxt->cibase = (mrb_callinfo *)((((uintptr_t)(cxt->cibase_org)) & (~(64 - 1))) + 64);
    cxt->ci = cxt->cibase + size;
    cxt->ciend = cxt->cibase + size * 2;
  }
  ci = ++cxt->ci;
  ci->ridx = ridx;
  ci->env = 0;
  ci->jit_entry = NULL;

  ci->mid = mid;
  ci->proc = m;
  ci->stackent = mrb->c->stack;
  ci->argc = n;
  if (c->tt == MRB_TT_ICLASS) {
    ci->target_class = c->c;
  }
  else {
    ci->target_class = c;
  }
  ci->pc = pc + 1;
  ci->acc = a;

  /* prepare stack */
  mrb->c->stack += a;

  /* setup environment for calling method */
  *status->proc = mrb->c->ci->proc = m;
  irep = *status->irep = m->body.irep;
  ci->nregs = irep->nregs;
  if (mrb->c->stack + irep->nregs >= mrb->c->stend) {
    mrbjit_stack_extend(mrb, irep->nregs);
  }
  *status->pc = irep->iseq;

  return NULL;
}

void *
mrbjit_exec_enter(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  mrb_code i = *pc;
  mrb_irep *irep = *status->irep;
  struct RProc *proc = *status->proc;

  //printf("enter %x %x \n", pc, mrb->c->ci);
  /* Ax             arg setup according to flags (23=5:5:1:5:5:1:1) */
  /* number of optional arguments times OP_JMP should follow */
  mrb_aspec ax = GETARG_Ax(i);
  int m1 = MRB_ASPEC_REQ(ax);
  int o  = MRB_ASPEC_OPT(ax);
  int r  = MRB_ASPEC_REST(ax);
  int m2 = MRB_ASPEC_POST(ax);
  /* unused
     int k  = (ax>>2)&0x1f;
     int kd = (ax>>1)&0x1;
     int b  = (ax>>0)& 0x1;
  */
  int argc = mrb->c->ci->argc;
  int oargc = mrb->c->ci->argc;
  mrb_value *argv = regs+1;
  mrb_value *argv0 = argv;
  int len = m1 + o + r + m2;
  mrb_value *blk = &argv[argc < 0 ? 1 : argc];

  if (argc < 0) {
    struct RArray *ary = mrb_ary_ptr(regs[1]);
    argv = ARY_PTR(ary);
    argc = ARY_LEN(ary);
    mrb_gc_protect(mrb, regs[1]);
  }
  if (mrb->c->ci->proc && MRB_PROC_STRICT_P(mrb->c->ci->proc)) {
    if (argc >= 0) {
      if (argc < m1 + m2 || (r == 0 && argc > len)) {
	mrbjit_argnum_error(mrb, m1+m2);
	return status->gototable[0]; /* L_RAISE */
      }
    }
  }
  else if (len > 1 && argc == 1 && mrb_array_p(argv[0])) {
    mrb_gc_protect(mrb, argv[0]);
    argc = ARY_LEN(mrb_ary_ptr(argv[0]));
    argv = ARY_PTR(mrb_ary_ptr(argv[0]));
  }
  if (argc < len) {
    int mlen = m2;
    if (argc < m1+m2) {
      if (m1 < argc)
	mlen = argc - m1;
      else
	mlen = 0;
    }
    regs[len+1] = *blk; /* move block */
    SET_NIL_VALUE(regs[argc+1]);
    if (argv0 != argv) {
      value_move(&regs[1], argv, argc-mlen); /* m1 + o */
    }
    if (argc < m1) {
      mrbjit_stack_clear(mrb, &regs[argc+1], m1-argc);
    }
    if (mlen) {
      value_move(&regs[len-m2+1], &argv[argc-mlen], mlen); /* m2 */
    }
    if (mlen < m2) {
      mrbjit_stack_clear(mrb, &regs[len-m2+mlen+1], m2-mlen);
    }
    if (r) {                  /* r */
      regs[m1+o+1] = mrb_ary_new_capa(mrb, 0);
    }
    if (o == 0 || argc < m1+m2) {
      *(status->pc) += 1;
    }
    else
      *(status->pc) += argc - m1 - m2 + 1;
  }
  else {
    int rnum = 0;
    if (argv0 != argv) {
      regs[len+1] = *blk; /* move block */
      value_move(&regs[1], argv, m1+o); /* m1 + o */
    }
    if (r) {                  /* r */
      rnum = argc - m1 - o - m2;
      //printf("%d %x\n", rnum, irep);
      //disasm_irep(mrb, irep);
      if (rnum == 1 && oargc >= 0) {
	int ipos = 0;
	mrb_irep *nirep = (mrb_irep *)((intptr_t)mrb + mrb_fixnum(irep->pool[ipos]));
	struct RProc *p;

	if (nirep == (mrb_irep *)mrb) {
	  int i;
	  mrb_irep *cirep = mrb_add_irep(mrb);
	  *cirep = *irep;
	  cirep->jit_entry_tab = (mrbjit_codetab *)mrb_calloc(mrb, irep->ilen, sizeof(mrbjit_codetab));
	  for (i = 0; i < cirep->ilen; i++) {
	    cirep->jit_entry_tab[i].size = 2;
	    cirep->jit_entry_tab[i].body = 
	      (mrbjit_code_info *)mrb_calloc(mrb, 2, sizeof(mrbjit_code_info));
	  }
	  cirep->prof_info = (int *)mrb_calloc(mrb, 1, sizeof(int)*irep->ilen);
	  if (mrb_patch_irep_var2fix(mrb, cirep, m1 + o + 1)) {
	    if (irep->shared_lambda == 1) {
	      p = mrbjit_get_local_proc(mrb, cirep);
	    }
	    else {
	      p = mrb_proc_new(mrb, cirep);
	    }
	    p->flags = proc->flags;
	    p->body.irep->refcnt++;
	    p->e.env = proc->e.env;
	    p->upper = proc->upper;
	    (*status->irep)->pool[ipos] = mrb_fixnum_value(((intptr_t)cirep - (intptr_t)mrb));
	    mrb->c->ci->proc = proc = p;
	    *status->irep = cirep;
	    *status->pc = cirep->iseq;
	    //assert(p->env == NULL || p->env->cioff >= 0);
	  }
	  else {
	    regs[m1+o+1] = mrb_ary_new_from_values(mrb, rnum, argv+m1+o);
	  }
	}
	else {
	  if (irep->shared_lambda == 1) {
	    p = mrbjit_get_local_proc(mrb, nirep);
	  }
	  else {
	    p = mrb_proc_new(mrb, nirep);
	  }
	  p->flags = proc->flags;
	  p->body.irep->refcnt++;
	  p->e.env = proc->e.env;
	  p->upper = proc->upper;
	  mrb->c->ci->proc = proc = p;
	  *status->irep = nirep;
	  *status->pc = nirep->iseq;
	  //	      assert(p->e.env == NULL || p->e.env->cioff >= 0);
	}
      }
      else {
	regs[m1+o+1] = mrb_ary_new_from_values(mrb, rnum, argv+m1+o);
      }
    }
    if (m2) {
      if (argc-m2 > m1) {
	value_move(&regs[m1+o+r+1], &argv[m1+o+rnum], m2);
      }
    }
    if (argv0 == argv) {
      regs[len+1] = *blk; /* move block */
    }
    *(status->pc) += o + 1;
  }

  mrb->c->ci->argc = len;
  /* clear local (but non-argument) variables */
  if ((int)(irep->nlocals-len-2) > 0) {
    mrbjit_stack_clear(mrb, &regs[len+2], irep->nlocals-len-2);
  }
  if (o == 0) {
    return NULL;
  }

  return status->gototable[6]; /* goto L_DISPATCH */
}

void mrb_exc_set(mrb_state *mrb, mrb_value exc);

void *
mrbjit_exec_return(mrb_state *mrb, mrbjit_vmstatus *status)
{
  /* A B     return R(A) (B=normal,in-block return/break) */
  mrb_code i = **(status->pc);
  mrb_callinfo *ci;
  void *rc = NULL;
  mrb_irep *irep;
  struct RProc *proc = *status->proc;

  ci = mrb->c->ci;
  if (ci->mid) {
    mrb_value blk;

    if (ci->argc < 0) {
      blk = (mrb->c->stack)[2];
    }
    else {
      blk = (mrb->c->stack)[ci->argc+1];
    }
    if (mrb_type(blk) == MRB_TT_PROC) {
      struct RProc *p = mrb_proc_ptr(blk);

      if (!MRB_PROC_STRICT_P(p) &&
	  ci > mrb->c->cibase && p->e.env == ci[-1].env) {
	p->flags |= MRB_PROC_ORPHAN;
      }
    }
  }

  if (mrb->exc) {
    mrb_callinfo *ci0;

  L_RAISE:
    ci0 = ci = mrb->c->ci;
    if (ci == mrb->c->cibase) {
      if (ci->ridx == 0) goto L_FTOP;
      goto L_RESCUE;
    }
    while (ci[0].ridx == ci[-1].ridx) {
      mrbjit_cipop(mrb);
      mrb->c->stack = ci->stackent;
      if (ci->acc == CI_ACC_SKIP && *status->prev_jmp) {
	mrb->jmp = *status->prev_jmp;
	MRB_THROW(*status->prev_jmp);
      }
      ci = mrb->c->ci;
      if (ci == mrb->c->cibase) {
	if (ci->ridx == 0) {
	L_FTOP:             /* fiber top */
	  if (mrb->c == mrb->root_c) {
	    mrb->c->stack = mrb->c->stbase;
	    return status->gototable[4]; /* L_STOP */
	  }
	  else {
	    struct mrb_context *c = mrb->c;

	    while (mrb->c->eidx > ci->epos) {
	      ptrdiff_t cioff = ci - mrb->c->cibase;\
	      mrbjit_ecall(mrb);			    \
	      ci = mrb->c->cibase + cioff;\
	    }
	    if (c->fib) {
	      mrb_write_barrier(mrb, (struct RBasic*)c->fib);
	    }
	    mrb->c->status = MRB_FIBER_TERMINATED;
	    mrb->c = c->prev;
	    c->prev = NULL;
	    goto L_RAISE;
	  }
	}
	break;
      }
      /* call ensure only when we skip this callinfo */
      if (ci[0].ridx == ci[-1].ridx) {
	while (mrb->c->eidx > ci->epos) {
	  ptrdiff_t cioff = ci - mrb->c->cibase;
	  mrbjit_ecall(mrb);
	  ci = mrb->c->cibase + cioff;
	}
      }
    }

  L_RESCUE:
    if (ci->ridx == 0) 	return status->gototable[4]; /* L_STOP */
    proc = *(status->proc) = ci->proc;
    irep = *status->irep = proc->body.irep;
    *(status->pool) = irep->pool;
    *(status->syms) = irep->syms;
    if (ci < ci0) {
      mrb->c->stack = ci[1].stackent;
    }
    mrb->c->ci->prev_pc = NULL;
    mrbjit_stack_extend(mrb, irep->nregs);
    *(status->pc) = mrb->c->rescue[--ci->ridx];
  }
  else {
    int acc;
    mrb_value v;
    struct RProc *dst;

    ci = mrb->c->ci;
    v = (mrb->c->stack)[GETARG_A(i)];
    mrb_gc_protect(mrb, v);
    switch (GETARG_B(i)) {
    case OP_R_RETURN:
      /* Fall through to OP_R_NORMAL otherwise */
      if (ci->acc >=0 && MRB_PROC_ENV_P(proc) && !MRB_PROC_STRICT_P(proc)) {
	mrb_callinfo *cibase = mrb->c->cibase;
	dst = mrbjit_top_proc(mrb, proc);

	if (MRB_PROC_ENV_P(dst)) {
	  struct REnv *e = MRB_PROC_ENV(dst);

	  if (!MRB_ENV_STACK_SHARED_P(e) || e->cxt != mrb->c) {
	    mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	    goto L_RAISE;
	  }
	}
	while (cibase <= ci && ci->proc->body.irep->org_iseq != dst->body.irep->org_iseq) {
	  if (ci->acc < 0) {
	    mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	    goto L_RAISE;
	  }
	  ci--;
	}
	if (ci <= cibase) {
	  mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	  goto L_RAISE;
	}
	break;
      }
    case OP_R_NORMAL:
    NORMAL_RETURN:
      if (ci == mrb->c->cibase) {
	struct mrb_context *c;

	if (!mrb->c->prev) { /* toplevel return */
	  mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	  goto L_RAISE;
	}
	if (mrb->c->prev->ci == mrb->c->prev->cibase) {
	  mrb_value exc = mrb_exc_new_str_lit(mrb, E_FIBER_ERROR, "double resume");
	  mrb_exc_set(mrb, exc);
	  goto L_RAISE;
	}
	while (mrb->c->eidx > 0) {
	  mrbjit_ecall(mrb);
	}
	/* automatic yield at the end */
	c = mrb->c;
	c->status = MRB_FIBER_TERMINATED;
	mrb->c = c->prev;
	c->prev = NULL;
	mrb->c->status = MRB_FIBER_RUNNING;
	ci = mrb->c->ci;
      }
      break;
    case OP_R_BREAK:
      if (MRB_PROC_STRICT_P(proc)) goto NORMAL_RETURN;
      if (MRB_PROC_ORPHAN_P(proc)) { 
	mrb_value exc;

      L_BREAK_ERROR:
	exc = mrb_exc_new_str_lit(mrb, E_LOCALJUMP_ERROR,
				  "break from proc-closure");
	mrb_exc_set(mrb, exc);
	goto L_RAISE;
      }
      if (!proc->e.env || !MRB_ENV_STACK_SHARED_P(proc->e.env)) {
	goto L_BREAK_ERROR;
      }
      else {
	struct REnv *e = MRB_PROC_ENV(proc);

	if (e == mrb->c->cibase->env && proc->body.irep->org_iseq != mrb->c->cibase->proc->body.irep->org_iseq) {
	  goto L_BREAK_ERROR;
	}
	if (e->cxt != mrb->c) {
	  goto L_BREAK_ERROR;
	}
      }
      while (mrb->c->eidx > mrb->c->ci->epos) {
	ptrdiff_t cioff = ci - mrb->c->cibase;
	mrbjit_ecall(mrb);
	ci = mrb->c->cibase + cioff;
      }
      /* break from fiber block */
      if (mrb->c->ci == mrb->c->cibase && mrb->c->ci->pc) {
	struct mrb_context *c = mrb->c;

	mrb->c = c->prev;
	c->prev = NULL;
	ci = mrb->c->ci;
      }
      if (ci->acc < 0) {
	ARENA_RESTORE(mrb, ai);
	mrb->c->vmexec = FALSE;
	mrb->exc = (struct RObject*)mrbjit_break_new(mrb, proc, v);
	mrb->jmp = *status->prev_jmp;
	MRB_THROW(*status->prev_jmp);
      }
      if (FALSE) {
	v = ((struct RBreak*)mrb->exc)->val;
	proc = ((struct RBreak*)mrb->exc)->proc;
	mrb->exc = NULL;
	ci = mrb->c->ci;
      }
      mrb->c->stack = ci->stackent;
      proc = proc->upper;
      while (mrb->c->cibase < ci &&  ci[-1].proc->body.irep->org_iseq != proc->body.irep->org_iseq) {
	if (ci[-1].acc == CI_ACC_SKIP) {
	  while (ci < mrb->c->ci) {
	    mrbjit_cipop(mrb);
	  }
	  goto L_BREAK_ERROR;
	}
	ci--;
      }
      if (ci == mrb->c->cibase) {
	goto L_BREAK_ERROR;
      }
      break;
    default:
      /* cannot happen */
      break;
    }
    while (ci < mrb->c->ci) {
      mrbjit_cipop(mrb);
    }
    ci[0].ridx = ci[-1].ridx;
    while (mrb->c->eidx > mrb->c->ci->epos) {
      ptrdiff_t cioff = ci - mrb->c->cibase;
      mrbjit_ecall(mrb);
      ci = mrb->c->cibase + cioff;
    }
    if (mrb->c->vmexec && !mrb->c->ci->target_class) {
      ARENA_RESTORE(mrb, ai);
      mrb->c->vmexec = FALSE;
      mrb->jmp = *status->prev_jmp;
      (mrb->c->stack)[(*status->irep)->nlocals] = v;
      return status->gototable[5]; /* L_HALT */
    }
    ci = mrb->c->ci;
    acc = ci->acc;
    mrb->c->stack = ci->stackent;
    mrbjit_cipop(mrb);
    if (acc == CI_ACC_SKIP || acc == CI_ACC_DIRECT) {
      ARENA_RESTORE(mrb, ai);
      mrb->jmp = *status->prev_jmp;
      (mrb->c->stack)[(*status->irep)->nlocals] = v;
      return status->gototable[5]; /* L_HALT */
    }
    *(status->pc) = ci->pc;
    DEBUG(fprintf(stderr, "from :%s\n", mrb_sym2name(mrb, ci->mid)));
    proc = *(status->proc) = mrb->c->ci->proc;
    irep = *(status->irep) = proc->body.irep;
    *(status->pool) = irep->pool;
    *(status->syms) = irep->syms;

    (mrb->c->stack)[acc] = v;
    ARENA_RESTORE(mrb, ai);
  }

  return rc;
}

void *
mrbjit_exec_return_fast(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code i = **(status->pc);
  mrb_callinfo *ci;
  void *rc = NULL;
  mrb_irep *irep;
  struct RProc *proc = *status->proc;

  /* A      return R(A) */
  if (mrb->exc) {
    mrb_callinfo *ci0;

  L_RAISE:
    ci0 = ci = mrb->c->ci;
    if (ci == mrb->c->cibase) {
      if (ci->ridx == 0) goto L_FTOP;
      goto L_RESCUE;
    }
    while (ci[0].ridx == ci[-1].ridx) {
      mrbjit_cipop(mrb);
      mrb->c->stack = ci->stackent;
      if (ci->acc == CI_ACC_SKIP && *status->prev_jmp) {
	mrb->jmp = *status->prev_jmp;
	MRB_THROW(*status->prev_jmp);
      }
      ci = mrb->c->ci;
      if (ci == mrb->c->cibase) {
	if (ci->ridx == 0) {
	L_FTOP:             /* fiber top */
	  if (mrb->c == mrb->root_c) {
	    mrb->c->stack = mrb->c->stbase;
	    return status->gototable[4]; /* L_STOP */
	  }
	  else {
	    struct mrb_context *c = mrb->c;

	    while (mrb->c->eidx > ci->epos) {
	      ptrdiff_t cioff = ci - mrb->c->cibase;\
	      mrbjit_ecall(mrb);			    \
	      ci = mrb->c->cibase + cioff;\
	    }
	    if (c->fib) {
	      mrb_write_barrier(mrb, (struct RBasic*)c->fib);
	    }
	    mrb->c->status = MRB_FIBER_TERMINATED;
	    mrb->c = c->prev;
	    c->prev = NULL;
	    goto L_RAISE;
	  }
	}
	break;
      }
      /* call ensure only when we skip this callinfo */
      if (ci[0].ridx == ci[-1].ridx) {
	while (mrb->c->eidx > ci->epos) {
	  ptrdiff_t cioff = ci - mrb->c->cibase;
	  mrbjit_ecall(mrb);
	  ci = mrb->c->cibase + cioff;
	}
      }
    }

  L_RESCUE:
    if (ci->ridx == 0) 	return status->gototable[4]; /* L_STOP */
    proc = *(status->proc) = ci->proc;
    irep = *status->irep = proc->body.irep;
    *(status->pool) = irep->pool;
    *(status->syms) = irep->syms;
    if (ci < ci0) {
      mrb->c->stack = ci[1].stackent;
    }
    mrb->c->ci->prev_pc = NULL;
    mrbjit_stack_extend(mrb, irep->nregs);
    *(status->pc) = mrb->c->rescue[--ci->ridx];
  }
  else {
    int acc;
    mrb_value v;

    ci = mrb->c->ci;
    v = (mrb->c->stack)[GETARG_A(i)];
    mrb_gc_protect(mrb, v);
    if (ci == mrb->c->cibase) {
      struct mrb_context *c;

      if (!mrb->c->prev) { /* toplevel return */
	mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	goto L_RAISE;
      }
      if (mrb->c->prev->ci == mrb->c->prev->cibase) {
	mrb_value exc = mrb_exc_new_str_lit(mrb, E_FIBER_ERROR, "double resume");
	mrb_exc_set(mrb, exc);
	goto L_RAISE;
      }
      while (mrb->c->eidx > 0) {
	mrbjit_ecall(mrb);
      }
      /* automatic yield at the end */
      c = mrb->c;
      c->status = MRB_FIBER_TERMINATED;
      mrb->c = c->prev;
      c->prev = NULL;
      mrb->c->status = MRB_FIBER_RUNNING;
      ci = mrb->c->ci;
    }
    while (ci < mrb->c->ci) {
      mrbjit_cipop(mrb);
    }
    ci[0].ridx = ci[-1].ridx;
    while (mrb->c->eidx > mrb->c->ci->epos) {
      ptrdiff_t cioff = ci - mrb->c->cibase;
      mrbjit_ecall(mrb);
      ci = mrb->c->cibase + cioff;
    }
    if (mrb->c->vmexec && !mrb->c->ci->target_class) {
      ARENA_RESTORE(mrb, ai);
      mrb->c->vmexec = FALSE;
      mrb->jmp = *status->prev_jmp;
      (mrb->c->stack)[(*status->irep)->nlocals] = v;
      return status->gototable[5]; /* L_HALT */
    }
    ci = mrb->c->ci;
    acc = ci->acc;
    mrb->c->stack = ci->stackent;
    mrbjit_cipop(mrb);
    if (acc == CI_ACC_SKIP || acc == CI_ACC_DIRECT) {
      ARENA_RESTORE(mrb, ai);
      mrb->jmp = *status->prev_jmp;
      (mrb->c->stack)[(*status->irep)->nlocals] = v;
      return status->gototable[5]; /* L_HALT */
    }
    *(status->pc) = ci->pc;
    DEBUG(fprintf(stderr, "from :%s\n", mrb_sym2name(mrb, ci->mid)));
    proc = *(status->proc) = mrb->c->ci->proc;
    irep = *(status->irep) = proc->body.irep;
    *(status->pool) = irep->pool;
    *(status->syms) = irep->syms;

    (mrb->c->stack)[acc] = v;
    ARENA_RESTORE(mrb, ai);
  }

  return rc;
}

void *
mrbjit_exec_call(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_callinfo *ci;
  mrb_value *stack = mrb->c->stack; 
  mrb_value recv = stack[0];
  struct RProc *m = mrb_proc_ptr(recv);
  struct REnv *env = m->e.env;
  mrb_irep *irep = m->body.irep;

  /* replace callinfo */
  ci = mrb->c->ci;
  ci->proc = m;
  ci->nregs = irep->nregs;

  stack[0] = env->stack[0];

  /* setup environment for calling method */
  *status->proc = m;
  *status->irep = irep;
  *status->pc = irep->iseq;

  return NULL;
}

void
mrbjit_reset_irep_mild(mrb_state *mrb, mrbjit_vmstatus *status, mrb_irep *irep)
{
  /* Disable JIT output code for redefined method */
  mrbjit_codetab *tab = irep->jit_entry_tab;
  mrbjit_code_info *entry;
  int i;

  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->used > 0) {
      mrbjit_code_area cbase = mrb->compile_info.code_base;
      mrbjit_gen_exit_patch(cbase, mrb, (void *)entry->entry, irep->iseq, status, NULL);
    }
  }
}

void
mrbjit_reset_irep(mrb_state *mrb, mrbjit_vmstatus *status, mrb_irep *irep)
{
  /* Disable JIT output code for redefined method */
  mrbjit_codetab *tab = irep->jit_entry_tab;
  mrbjit_code_info *entry;
  int i;

  irep->entry = NULL;
  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->used > 0) {
      mrbjit_code_area cbase = mrb->compile_info.code_base;
      mrbjit_gen_exit_patch2(cbase, mrb, (void *)entry->entry, irep->iseq, status, NULL);
    }
  }
}

static void
mrbjit_reset_caller_aux(mrb_state *mrb, mrbjit_vmstatus *status)
{
  /* Disable JIT output code for redefined method of caller */
  struct RProc *p = mrb->c->ci[-1].proc;
  mrb_irep *irep = p->body.irep;
  mrbjit_codetab *tab = irep->jit_entry_tab;
  mrbjit_code_info *entry;
  int i;

  if (tab) {
    for (i = 0; i < tab->size; i++) {
      entry = tab->body + i;
      if (entry->used > 0) {
	mrbjit_code_area cbase = mrb->compile_info.code_base;
	if (irep->block_lambda) {
	  mrbjit_gen_exit_patch(cbase, mrb, (void *)entry->entry, NULL, status, NULL);
	}
	else {
	  mrbjit_gen_exit_patch(cbase, mrb, (void *)entry->entry, irep->iseq, status, NULL);
	}
      }
    }
  }
  irep->entry = NULL;
  if (irep->jit_entry_tab) {
    int i;
    int j;

    for (i = 0; i < irep->ilen; i++) {
      for (j = 0; j < irep->jit_entry_tab[i].size; j++) {
	if (irep->jit_entry_tab[i].body[j].reginfo) {
	  mrb_free(mrb, irep->jit_entry_tab[i].body[j].reginfo);
	}
      }
    }
    mrb_free(mrb, irep->jit_entry_tab->body);
    mrb_free(mrb, irep->jit_entry_tab);
    irep->jit_entry_tab = NULL;
  }
}

void
mrbjit_reset_caller()
{
  mrb_state *mrb;
  mrbjit_vmstatus status;
  struct RProc *p;
  mrb_irep *irep;

#ifdef __i386__
  asm volatile("mov %%esi, %0\n\t"
	       : "=c"(mrb));
  asm volatile("mov %%ebx, %0\n\t"
	       : "=c"(status.pc));
#elif __x86_64__
  asm volatile("mov %%r13, %0\n\t"
	       : "=c"(mrb));
  asm volatile("mov %%rbx, %0\n\t"
	       : "=c"(status.pc));
#endif
  p = mrb->c->ci[-1].proc;
  /*  If current proc is still alive ... */
  if (p->tt == MRB_TT_PROC) {
    irep = p->body.irep;
    status.irep = &irep;
    mrbjit_reset_caller_aux(mrb, &status);
  }
}

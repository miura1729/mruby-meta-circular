/*
** jit.c - Toplevel of JIT
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "opcode.h"
#include "error.h"
#include "value_array.h"
#include "mruby/jit.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "mruby/proc.h"
#include "mruby/class.h"
#include "mruby/array.h"
#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>

#ifdef JIT_DEBUG
# define DEBUG(x) (x)
#else
# define DEBUG(x)
#endif

int
mrbjit_check_inlineble(mrb_state *mrb, mrb_irep *irep)
{
  int i;

  if (irep->ilen >= NO_INLINE_METHOD_LEN) {
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
      //case OP_SENDB:
      //    case OP_SEND:
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
    case OP_GETUPVAR:
    case OP_SETUPVAR:
    case OP_JMP:
    case OP_JMPIF:
    case OP_JMPNOT:
      //case OP_LAMBDA:
    case OP_RANGE:
    case OP_STRING:
    case OP_STRCAT:
    case OP_HASH:
    case OP_BLKPUSH:
      break;

    default:
      return 0;
    }
  }

  return 1;
}

void *
mrbjit_exec_send_c(mrb_state *mrb, mrbjit_vmstatus *status,
		 struct RProc *m, struct RClass *c)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_value *regs = *status->regs;
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

  ci->proc = m;
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

  //mrb_p(mrb, recv);
  mrb->compile_info.disable_jit = 1;
  result = m->body.func(mrb, recv);
  mrb->compile_info.disable_jit = orgdisflg;
  mrb->c->stack[0] = result;
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) {
    ci->mid = mid;
    ci->proc = m;
    return status->gototable[0];	/* goto L_RAISE; */
  }
  /* pop stackpos */
  ci = mrb->c->ci;
  if (!ci->target_class) { /* return from context modifying method (resume/yield) */
    if (!MRB_PROC_CFUNC_P(ci[-1].proc)) {
      irep = *(status->irep) = ci[-1].proc->body.irep;
      *(status->pool) = irep->pool;
      *(status->syms) = irep->syms;
    }
    *(status->regs) = mrb->c->stack = mrb->c->ci->stackent;
    mrbjit_cipop(mrb);
    *(status->pc) = ci->pc;

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
  cxt->cibase = (mrb_callinfo *)(((int)(cxt->cibase_org) + 63) & (~(64 - 1)));
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
  int eidx = ci->eidx;
  int ridx = ci->ridx;

  if (ci + 1 == cxt->ciend) {
    size_t size = ci - cxt->cibase;

    cxt->cibase_org = (mrb_callinfo *)mrb_realloc(mrb, cxt->cibase_org, sizeof(mrb_callinfo)*size*2 + 64);
    cxt->cibase = (mrb_callinfo *)((((int)(cxt->cibase_org)) & (~(64 - 1))) + 64);
    cxt->ci = cxt->cibase + size;
    cxt->ciend = cxt->cibase + size * 2;
  }
  ci = ++cxt->ci;
  ci->eidx = eidx;
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
    mrbjit_stack_extend(mrb, irep->nregs,  ci->argc+2);
  }
  *status->regs = mrb->c->stack;
  *status->pc = irep->iseq;

  return NULL;
}

void *
mrbjit_exec_enter(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = *status->regs;
  mrb_code i = *pc;

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
  mrb_value *argv = regs+1;
  mrb_value *argv0 = argv;
  int len = m1 + o + r + m2;
  mrb_value *blk = &argv[argc < 0 ? 1 : argc];

  if (!mrb_nil_p(*blk) && mrb_type(*blk) != MRB_TT_PROC) {
    *blk = mrb_convert_type(mrb, *blk, MRB_TT_PROC, "Proc", "to_proc");
  }
  if (argc < 0) {
    struct RArray *ary = mrb_ary_ptr(regs[1]);
    argv = ary->ptr;
    argc = ary->len;
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
    argc = mrb_ary_ptr(argv[0])->len;
    argv = mrb_ary_ptr(argv[0])->ptr;
  }
  mrb->c->ci->argc = len;
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
    if (mlen) {
      value_move(&regs[len-m2+1], &argv[argc-mlen], m2); /* m2 */
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
    if (argv0 != argv) {
      regs[len+1] = *blk; /* move block */
      value_move(&regs[1], argv, m1+o); /* m1 + o */
    }
    if (r) {                  /* r */
      regs[m1+o+1] = mrb_ary_new_from_values(mrb, argc-m1-o-m2, argv+m1+o);
    }
    if (m2) {
      if (argc-m2 > m1) {
	value_move(&regs[m1+o+r+1], &argv[argc-m2], m2);
      }
    }
    if (argv0 == argv) {
      regs[len+1] = *blk; /* move block */
    }
    *(status->pc) += o + 1;
    if (o == 0) {
      return NULL;
    }
  }

  return status->optable[GET_OPCODE(**(status->pc))];
}

void *
mrbjit_exec_return(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code i = **(status->pc);
  void *rc = NULL;

  //printf("return %x\n", *status->irep);
  //printf("rc %x %x %s\n", *status->pc, mrb->c->ci, mrb_sym2name(mrb, mrb->c->ci->mid));
  //printf("%x\n", mrb->c->ci->jit_entry);

  /* A      return R(A) */
  if (mrb->exc) {
    mrb_callinfo *ci = mrb->c->ci;
    int eidx;

  L_RAISE:
    ci = mrb->c->ci;
    mrb_obj_iv_ifnone(mrb, mrb->exc, mrb_intern_lit(mrb, "lastpc"), mrb_cptr_value(mrb, *status->pc));
    mrb_obj_iv_set(mrb, mrb->exc, mrb_intern_lit(mrb, "ciidx"), mrb_fixnum_value(ci - mrb->c->cibase));
    eidx = ci->eidx;
    if (ci == mrb->c->cibase) {
      if (ci->ridx == 0) {
	return status->gototable[4]; /* L_STOP */
      }
      else {
	return status->gototable[2]; /* L_RESCUE */
      }
    }
    while (eidx > ci[-1].eidx) {
      mrbjit_ecall(mrb, --eidx);
    }
    while (ci[0].ridx == ci[-1].ridx) {
      mrbjit_cipop(mrb);
      ci = mrb->c->ci;
      mrb->c->stack = ci[1].stackent;
      if (ci[1].acc < 0 && *status->prev_jmp) {
	mrb->jmp = *status->prev_jmp;
	longjmp(*(jmp_buf*)mrb->jmp, 1);
      }
      if (ci > mrb->c->cibase) {
	while (eidx > mrb->c->ci->eidx) {
	  mrbjit_ecall(mrb, --eidx);
	}
      }
      else if (ci == mrb->c->cibase) {
	if (ci->ridx == 0) {
	  *status->regs = mrb->c->stack = mrb->c->stbase;
	  return status->gototable[4]; /* L_STOP */
	}
	else {
	  struct mrb_context *c = mrb->c;

	  mrb->c = c->prev;
	  c->prev = NULL;
	  return status->gototable[0];	/* goto L_RAISE; */
	}
	break;
      }
    }
    if (ci->ridx == 0) {
      return status->gototable[4]; /* L_STOP */
    }
    *status->proc = ci->proc;
    *status->irep = ci->proc->body.irep;
    *status->regs = mrb->c->stack = ci[1].stackent;
    *status->pc = mrb->c->rescue[--ci->ridx];
  }
  else {
    mrb_callinfo *ci = mrb->c->ci;

    int acc, eidx = mrb->c->ci->eidx;
    mrb_value v = (*status->regs)[GETARG_A(i)];

    switch (GETARG_B(i)) {
    case OP_R_RETURN:
      // Fall through to OP_R_NORMAL otherwise
      if ((*status->proc)->env && !MRB_PROC_STRICT_P(*status->proc)) {
	struct REnv *e = mrbjit_top_env(mrb, *status->proc);

	if (e->cioff < 0) {
	  mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	  goto L_RAISE;
	}
	ci = mrb->c->cibase + e->cioff;
	if (ci == mrb->c->cibase) {
	  mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	  goto L_RAISE;
	}
	rc = status->optable[GET_OPCODE(*ci->pc)];
	mrb->c->ci = ci;
	break;
      }
    case OP_R_NORMAL:
      if (ci == mrb->c->cibase) {
	if (!mrb->c->prev) { /* toplevel return */
	  mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
	  goto L_RAISE;
	}
	if (mrb->c->prev->ci == mrb->c->prev->cibase) {
	  mrb_value exc = mrb_exc_new_str(mrb, E_RUNTIME_ERROR, mrb_str_new(mrb, "double resume", 13));
	  mrb->exc = mrb_obj_ptr(exc);
	  goto L_RAISE;
	}
	/* automatic yield at the end */
	mrb->c->status = MRB_FIBER_TERMINATED;
	mrb->c = mrb->c->prev;
      }
      ci = mrb->c->ci;
      break;
    case OP_R_BREAK:
      if ((*status->proc)->env->cioff < 0) {
	mrbjit_localjump_error(mrb, LOCALJUMP_ERROR_BREAK);
	goto L_RAISE;
      }
      ci = mrb->c->ci = mrb->c->cibase + (*status->proc)->env->cioff + 1;
      rc = status->optable[GET_OPCODE(*ci->pc)];
      break;
    default:
      /* cannot happen */
      break;
    }
    mrbjit_cipop(mrb);
    acc = ci->acc;
    *status->pc = ci->pc;
    *status->regs = mrb->c->stack = ci->stackent;
    while (eidx > mrb->c->ci->eidx) {
      mrbjit_ecall(mrb, --eidx);
    }
    if (acc < 0) {
      mrb->jmp = *status->prev_jmp;
      return rc;		/* return v */
    }
    DEBUG(printf("from :%s\n", mrb_sym2name(mrb, ci->mid)));
    *status->proc = mrb->c->ci->proc;
    *status->irep = (*status->proc)->body.irep;

    (*status->regs)[acc] = v;

    // return status->optable[GET_OPCODE(*ci->pc)];
  }

  return rc;
}

void *
mrbjit_exec_return_fast(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code i = **(status->pc);
  void *rc = NULL;

  /* A      return R(A) */
  if (mrb->exc) {
    mrb_callinfo *ci;
    int eidx;

    ci = mrb->c->ci;
    mrb_obj_iv_ifnone(mrb, mrb->exc, mrb_intern_lit(mrb, "lastpc"), mrb_cptr_value(mrb, *status->pc));
    mrb_obj_iv_set(mrb, mrb->exc, mrb_intern_lit(mrb, "ciidx"), mrb_fixnum_value(ci - mrb->c->cibase));
    eidx = ci->eidx;
    if (ci == mrb->c->cibase) {
      if (ci->ridx == 0) {
	return status->gototable[4]; /* L_STOP */
      }
      else {
	return status->gototable[2]; /* L_RESCUE */
      }
    }
    while (eidx > ci[-1].eidx) {
      mrbjit_ecall(mrb, --eidx);
    }
    while (ci[0].ridx == ci[-1].ridx) {
      mrbjit_cipop(mrb);
      ci = mrb->c->ci;
      mrb->c->stack = ci[1].stackent;
      if (ci[1].acc < 0 && *status->prev_jmp) {
	mrb->jmp = *status->prev_jmp;
	longjmp(*(jmp_buf*)mrb->jmp, 1);
      }
      if (ci > mrb->c->cibase) {
	while (eidx > mrb->c->ci->eidx) {
	  mrbjit_ecall(mrb, --eidx);
	}
      }
      else if (ci == mrb->c->cibase) {
	if (ci->ridx == 0) {
	  *status->regs = mrb->c->stack = mrb->c->stbase;
	  return status->gototable[4]; /* L_STOP */
	}
	else {
	  struct mrb_context *c = mrb->c;

	  mrb->c = c->prev;
	  c->prev = NULL;
	  return status->gototable[0];	/* goto L_RAISE; */
	}
	break;
      }
    }
    if (ci->ridx == 0) {
      return status->gototable[4]; /* L_STOP */
    }
    *status->proc = ci->proc;
    *status->irep = ci->proc->body.irep;
    *status->regs = mrb->c->stack = ci[1].stackent;
    *status->pc = mrb->c->rescue[--ci->ridx];
  }
  else {
    struct mrb_context *c = mrb->c;
    mrb_callinfo *ci = c->ci;

    int acc;
    int eidx = mrb->c->ci->eidx;
    mrb_value v = (*status->regs)[GETARG_A(i)];
    if (mrb->c->ci->env) {
      mrbjit_cipop(mrb);
    }
    else {
      c->ci--;
    }
    acc = ci->acc;
    *status->pc = ci->pc;
    *status->regs = c->stack = ci->stackent;
    while (eidx > c->ci->eidx) {
      mrbjit_ecall(mrb, --eidx);
    }
    if (acc < 0) {
      mrb->jmp = *status->prev_jmp;
      return rc;		/* return v */
    }
    DEBUG(printf("from :%s\n", mrb_sym2name(mrb, ci->mid)));
    *status->proc = c->ci->proc;
    *status->irep = (*status->proc)->body.irep;

    (*status->regs)[acc] = v;
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
  struct REnv *env = m->env;
  mrb_irep *irep = m->body.irep;

  /* replace callinfo */
  ci = mrb->c->ci;
  ci->target_class = m->target_class;
  ci->proc = m;
  ci->nregs = irep->nregs;

  if (env->mid) {
    ci->mid = env->mid;
  }
  stack[0] = env->stack[0];

  /* setup environment for calling method */
  *status->proc = m;
  *status->irep = irep;
  *status->pc = irep->iseq;

  return NULL;
}


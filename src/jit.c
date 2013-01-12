/*
** jit.c - Toplevel of JIT
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "opcode.h"
#include "mruby/jit.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "mruby/proc.h"
#include "mruby/class.h"
#include "mruby/array.h"
#include <stddef.h>
#include <stdio.h>
#include <setjmp.h>

extern const void *mrbjit_emit_code(mrb_state *, mrbjit_vmstatus *);
extern void mrbjit_gen_exit(mrbjit_code_area, mrb_state *, mrb_irep *, mrb_code **);
extern void mrbjit_gen_jump_block(mrbjit_code_area, void *);

static mrbjit_code_info *
add_codeinfo(mrb_state *mrb, mrbjit_codetab *tab)
{
  int i;
  int oldsize;
  mrbjit_code_info *ele;
  oldsize = -1;

 retry:
  if (tab->body == NULL || oldsize >= 0) {
    oldsize = tab->size;
    tab->size = tab->size + (tab->size >> 1) + 2;
    tab->body = mrb_realloc(mrb, tab->body, sizeof(mrbjit_code_info) * tab->size);
    for (i = oldsize; i < tab->size; i++) {
      tab->body[i].used = 0;
    }
  }

  oldsize = tab->size;
  for (i = 0; i < tab->size; i++) {
    ele = tab->body + i;
    if (ele->used == 0) {
      return ele;
    }
  }

  /* Grow code info table */
  goto retry;
}

mrbjit_code_info *
search_codeinfo_cbase(mrbjit_codetab *tab, mrbjit_code_area code_base)
{
  int i;
  mrbjit_code_info *entry;

  if (code_base == NULL) {
    return NULL;
  }

  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->code_base == code_base) {
      return entry;
    }
  }

  return NULL;
}

mrbjit_code_info *
search_codeinfo_prev(mrbjit_codetab *tab, mrb_code *prev_pc)
{
  int i;
  mrbjit_code_info *entry;

  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->prev_pc == prev_pc) {
      return entry;
    }
  }

  return NULL;
}

void *
mrbjit_dispatch(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_irep *irep = *status->irep;
  mrb_code **ppc = status->pc;
  mrb_value *regs = *status->regs;
  size_t n;
  mrbjit_code_info *ci;
  mrbjit_code_area cbase;
  mrb_code *prev_pc;

  if (mrb->compile_info.disable_jit) {
    return status->optable[GET_OPCODE(**ppc)];
  }

  prev_pc = mrb->compile_info.prev_pc;

  cbase = mrb->compile_info.code_base;
  n = ISEQ_OFFSET_OF(*ppc);
  if (prev_pc) {
    ci = search_codeinfo_prev(irep->jit_entry_tab + n, prev_pc);
  }
  else {
    ci = NULL;
  }

  if (cbase && ci && *ppc == prev_pc + 1) {
    if (ci->entry) {
      mrbjit_gen_jump_block(cbase, ci->entry);
    }
    else {
      mrbjit_gen_exit(cbase, mrb, irep, ppc);
    }

    cbase = mrb->compile_info.code_base = NULL;
  }

  if (ci && cbase == NULL) {
    if (ci->entry) {
      void *rc;

      //printf("%x %x \n", ci->entry, *ppc);
      asm("push %ecx");
      asm("mov %0, %%ecx"
	  :
	  : "a"(regs)
	  : "%ecx");
      asm("push %ebx");
      asm("mov %0, %%ebx"
	  :
	  : "a"(ppc)
	  : "%ebx");
      asm("push %0"
	  :
	  : "a"(status));

      ci->entry();

      asm("add $0x4, %esp");
      asm("pop %ebx");
      asm("pop %ecx");

      asm("mov %%eax, %0"
      	  : "=g"(rc));

      irep = *status->irep;
      regs = *status->regs;
      if (rc) {
	mrb->compile_info.prev_pc = *ppc;
	return rc;
      }
      //printf("%x %x \n", ci->entry, regs);
    }
  }
  else {
    void *(*entry)() = NULL;

    if (irep->prof_info[n]++ > COMPILE_THRESHOLD) {
      entry = mrbjit_emit_code(mrb, status);
      //      printf("size %x %x %x\n", irep->jit_entry_tab[n].size, *ppc, prev_pc);
      ci = add_codeinfo(mrb, irep->jit_entry_tab + n);
      ci->code_base = mrb->compile_info.code_base;
      ci->prev_pc = prev_pc;
      ci->used = 1;
      ci->entry = entry;
    }

    if (cbase && entry == NULL) {
      /* Finish compile */
      mrbjit_gen_exit(cbase, mrb, irep, ppc);
      mrb->compile_info.code_base = NULL;
    }
  }
  mrb->compile_info.prev_pc = *ppc;

  return status->optable[GET_OPCODE(**ppc)];
}

#define SET_NIL_VALUE(r) MRB_SET_VALUE(r, MRB_TT_FALSE, value.i, 0)
#define CALL_MAXARGS 127

void *
mrbjit_exec_send(mrb_state *mrb, mrbjit_vmstatus *status)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_irep *irep = *status->irep;
  mrb_value *regs = *status->regs;
  mrb_sym *syms = *status->syms;
  mrb_value *pool =  *status->pool;
  int ai = *status->ai;
  mrb_code i = *pc;

  int a = GETARG_A(i);
  int n = GETARG_C(i);
  struct RProc *m;
  struct RClass *c;
  mrb_callinfo *ci;
  mrb_value recv, result;
  mrb_sym mid = syms[GETARG_B(i)];
  int rcvoff = GETARG_Bx(*(pc + 1));
  int mthoff = rcvoff + 1;
  struct RClass *orecv = pool[rcvoff].value.p;

  recv = regs[a];
  if (GET_OPCODE(i) != OP_SENDB) {
    if (n == CALL_MAXARGS) {
      SET_NIL_VALUE(regs[a+2]);
    }
    else {
      SET_NIL_VALUE(regs[a+n+1]);
    }
  }
  c = mrb_class(mrb, recv);

  if (c != orecv) {
    m = mrb_method_search_vm(mrb, &c, mid);
    if (!m) {
      mrb_value sym = mrb_symbol_value(mid);

      mid = mrb_intern(mrb, "method_missing");
      m = mrb_method_search_vm(mrb, &c, mid);
      if (n == CALL_MAXARGS) {
	mrb_ary_unshift(mrb, regs[a+1], sym);
      }
      else {
	memmove(regs+a+2, regs+a+1, sizeof(mrb_value)*(n+1));

	regs[a+1] = sym;
	n++;
      }
    }
    else {
      mrb->is_method_cache_used = 1;
      irep->is_method_cache_used = 1;
      pool[rcvoff].value.p = c;
      pool[mthoff].value.p = m;
    }
  }
  else {
    m = pool[mthoff].value.p;
  }

  /* push callinfo */
  ci = mrbjit_cipush(mrb);
  ci->mid = mid;
  ci->proc = m;
  ci->stackidx = mrb->stack - mrb->stbase;
  ci->argc = n;
  if (ci->argc == CALL_MAXARGS) ci->argc = -1;
  ci->target_class = c;
  ci->pc = pc + 1;
  ci->acc = a;

  /* prepare stack */
  mrb->stack += a;

  // printf("%d %x %x %x %x %x\n", MRB_PROC_CFUNC_P(m), irep->iseq, pc,regs[a].ttt, regs, n);
  //puts(mrb_sym2name(mrb, mid));
  //mrb_p(mrb, recv);

  if (MRB_PROC_CFUNC_P(m)) {
    if (n == CALL_MAXARGS) {
      ci->nregs = 3;
    }
    else {
      ci->nregs = n + 2;
    }
    //mrb_p(mrb, recv);
    result = m->body.func(mrb, recv);
    mrb->stack[0] = result;
    mrb_gc_arena_restore(mrb, ai);
    if (mrb->exc) {
      return status->gototable[0];	/* goto L_RAISE; */
    }
    /* pop stackpos */
    regs = *status->regs = mrb->stack = mrb->stbase + mrb->ci->stackidx;
    mrbjit_cipop(mrb);
  }
  else {
    /* setup environment for calling method */
    *status->proc = mrb->ci->proc = m;
    irep = *status->irep = m->body.irep;
    pool = *status->pool = irep->pool;
    syms = *status->syms = irep->syms;
    ci->nregs = irep->nregs;
    if (ci->argc < 0) {
      mrbjit_stack_extend(mrb, (irep->nregs < 3) ? 3 : irep->nregs, 3);
    }
    else {
      mrbjit_stack_extend(mrb, irep->nregs,  ci->argc+2);
    }
    regs = *status->regs = mrb->stack;
    pc = *status->pc = irep->iseq;
  }

  return NULL;
}

void *
mrbjit_exec_enter(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = *status->regs;
  mrb_code i = *pc;

  /* Ax             arg setup according to flags (24=5:5:1:5:5:1:1) */
  /* number of optional arguments times OP_JMP should follow */
  int ax = GETARG_Ax(i);
  int m1 = (ax>>18)&0x1f;
  int o  = (ax>>13)&0x1f;
  int r  = (ax>>12)&0x1;
  int m2 = (ax>>7)&0x1f;
  /* unused
     int k  = (ax>>2)&0x1f;
     int kd = (ax>>1)&0x1;
     int b  = (ax>>0)& 0x1;
  */
  int argc = mrb->ci->argc;
  mrb_value *argv = regs+1;
  mrb_value *argv0 = argv;
  int len = m1 + o + r + m2;
  mrb_value *blk = &argv[argc < 0 ? 1 : argc];

  if (argc < 0) {
    struct RArray *ary = mrb_ary_ptr(regs[1]);
    argv = ary->ptr;
    argc = ary->len;
    mrb_gc_protect(mrb, regs[1]);
  }
  if (mrb->ci->proc && MRB_PROC_STRICT_P(mrb->ci->proc)) {
    if (argc >= 0) {
      if (argc < m1 + m2 || (r == 0 && argc > len)) {
	mrbjit_argnum_error(mrb, m1+m2);
	return status->gototable[0]; /* L_RAISE */
      }
    }
  }
  else if (len > 1 && argc == 1 && mrb_array_p(argv[0])) {
    argc = mrb_ary_ptr(argv[0])->len;
    argv = mrb_ary_ptr(argv[0])->ptr;
  }
  mrb->ci->argc = len;
  if (argc < len) {
    regs[len+1] = *blk; /* move block */
    if (argv0 != argv) {
      memmove(&regs[1], argv, sizeof(mrb_value)*(argc-m2)); /* m1 + o */
    }
    if (m2) {
      memmove(&regs[len-m2+1], &argv[argc-m2], sizeof(mrb_value)*m2); /* m2 */
    }
    if (r) {                  /* r */
      regs[m1+o+1] = mrb_ary_new_capa(mrb, 0);
    }
    if (o == 0) {
      *(status->pc) += 1;
      return NULL;
    }
    else
      *(status->pc) += argc - m1 - m2 + 1;
  }
  else {
    if (argv0 != argv) {
      memmove(&regs[1], argv, sizeof(mrb_value)*(m1+o)); /* m1 + o */
    }
    if (r) {                  /* r */
      regs[m1+o+1] = mrb_ary_new_elts(mrb, argc-m1-o-m2, argv+m1+o);
    }
    if (m2) {
      memmove(&regs[m1+o+r+1], &argv[argc-m2], sizeof(mrb_value)*m2);
    }
    regs[len+1] = *blk; /* move block */
    *(status->pc) += o + 1;
  }

  return status->optable[GET_OPCODE(**(status->pc))];
}

void *
mrbjit_exec_return(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code i = **(status->pc);

  /* A      return R(A) */
  if (mrb->exc) {
    mrb_callinfo *ci;
    int eidx;

  L_RAISE:
    ci = mrb->ci;
    mrb_obj_iv_ifnone(mrb, mrb->exc, mrb_intern(mrb, "lastpc"), mrb_voidp_value(*status->pc));
    mrb_obj_iv_set(mrb, mrb->exc, mrb_intern(mrb, "ciidx"), mrb_fixnum_value(ci - mrb->cibase));
    eidx = ci->eidx;
    if (ci == mrb->cibase) {
      if (ci->ridx == 0) {
	return status->gototable[4]; /* L_STOP */
      }
      else {
	return status->gototable[2]; /* L_RESCUE */
      }
    }
    while (ci[0].ridx == ci[-1].ridx) {
      mrbjit_cipop(mrb);
      ci = mrb->ci;
      if (ci[1].acc < 0 && *status->prev_jmp) {
	mrb->jmp = *status->prev_jmp;
	longjmp(*(jmp_buf*)mrb->jmp, 1);
      }
      while (eidx > mrb->ci->eidx) {
        mrbjit_ecall(mrb, --eidx);
      }
      if (ci == mrb->cibase) {
	if (ci->ridx == 0) {
	  *status->regs = mrb->stack = mrb->stbase;
	  return status->gototable[4]; /* L_STOP */
	}
	break;
      }
    }
    *status->irep = ci->proc->body.irep;
    *status->pool = (*status->irep)->pool;
    *status->syms = (*status->irep)->syms;
    *status->regs = mrb->stack = mrb->stbase + ci[1].stackidx;
    *status->pc = mrb->rescue[--ci->ridx];
  }
  else {
    mrb_callinfo *ci = mrb->ci;

    int acc, eidx = mrb->ci->eidx;
    mrb_value v = (*status->regs)[GETARG_A(i)];

    switch (GETARG_B(i)) {
    case OP_R_RETURN:
      // Fall through to OP_R_NORMAL otherwise
      if ((*status->proc)->env && !MRB_PROC_STRICT_P(*status->proc)) {
	struct REnv *e = mrbjit_top_env(mrb, *status->proc);

	if (e->cioff < 0) {
	  mrbjit_localjump_error(mrb, "return");
	  goto L_RAISE;
	}
	ci = mrb->cibase + e->cioff;
	if (ci == mrb->cibase) {
	  mrbjit_localjump_error(mrb, "return");
	  goto L_RAISE;
	}
	mrb->ci = ci;
	break;
      }
    case OP_R_NORMAL:
      if (ci == mrb->cibase) {
	mrbjit_localjump_error(mrb, "return");
	goto L_RAISE;
      }
      ci = mrb->ci;
      break;
    case OP_R_BREAK:
      if ((*status->proc)->env->cioff < 0) {
	mrbjit_localjump_error(mrb, "break");
	goto L_RAISE;
      }
      ci = mrb->ci = mrb->cibase + (*status->proc)->env->cioff + 1;
      break;
    default:
      /* cannot happen */
      break;
    }
    mrbjit_cipop(mrb);
    acc = ci->acc;
    *status->pc = ci->pc;
    *status->regs = mrb->stack = mrb->stbase + ci->stackidx;
    while (eidx > mrb->ci->eidx) {
      mrbjit_ecall(mrb, --eidx);
    }
    if (acc < 0) {
      mrb->jmp = *status->prev_jmp;
      return NULL;		/* return v */
    }
    DEBUG(printf("from :%s\n", mrb_sym2name(mrb, ci->mid)));
    *status->proc = mrb->ci->proc;
    *status->irep = (*status->proc)->body.irep;
    *status->pool = (*status->irep)->pool;
    *status->syms = (*status->irep)->syms;

    (*status->regs)[acc] = v;
  }

  printf("rc %x \n", *status->pc);
  return NULL;
}

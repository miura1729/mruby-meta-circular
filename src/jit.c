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

#ifdef JIT_DEBUG
# define DEBUG(x) (x)
#else
# define DEBUG(x)
#endif

mrbjit_code_info *
search_codeinfo_prev(mrbjit_codetab *tab, mrb_code *prev_pc, mrb_code *caller_pc)
{
  int i;
  mrbjit_code_info *entry;

  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->prev_pc == prev_pc && entry->caller_pc == caller_pc) {
      return entry;
    }
  }

  return NULL;
}

#define SET_NIL_VALUE(r) MRB_SET_VALUE(r, MRB_TT_FALSE, value.i, 0)

void *
mrbjit_exec_send_c(mrb_state *mrb, mrbjit_vmstatus *status,
		 struct RProc *m, struct RClass *c)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_value *regs = *status->regs;
  mrb_sym *syms = *status->syms;
  int ai = *status->ai;
  mrb_code i = *pc;

  int a = GETARG_A(i);
  int n = GETARG_C(i);
  mrb_callinfo *ci;
  mrb_value recv, result;
  mrb_sym mid = syms[GETARG_B(i)];
  int orgdisflg = mrb->compile_info.disable_jit;

  recv = regs[a];

  //printf("%d %x %x %x %x %x\n", MRB_PROC_CFUNC_P(m), irep->iseq, pc,regs[a].ttt, regs, n);
  //puts(mrb_sym2name(mrb, mid));

  ci = mrbjit_cipush(mrb);
  ci->stackidx = mrb->stack - mrb->stbase;
  ci->argc = n;
  ci->target_class = c;

  /* prepare stack */
  mrb->stack += a;

  ci->nregs = n + 2;
  //mrb_p(mrb, recv);
  mrb->compile_info.disable_jit = 1;
  result = m->body.func(mrb, recv);
  mrb->compile_info.disable_jit = orgdisflg;
  mrb->stack[0] = result;
  mrb_gc_arena_restore(mrb, ai);
  if (mrb->exc) {
    ci->mid = mid;
    ci->proc = m;
    return status->gototable[0];	/* goto L_RAISE; */
  }
  /* pop stackpos */
  mrb->stack = mrb->stbase + mrb->ci->stackidx;
  mrbjit_cipop(mrb);

  return NULL;
}

void *
mrbjit_exec_send_mruby(mrb_state *mrb, mrbjit_vmstatus *status,
		 struct RProc *m, struct RClass *c)
{
  /* A B C  R(A) := call(R(A),Sym(B),R(A+1),... ,R(A+C-1)) */
  mrb_code *pc = *status->pc;
  mrb_irep *irep = *status->irep;
  mrb_value *regs = *status->regs;
  mrb_sym *syms = *status->syms;
  mrb_code i = *pc;

  int a = GETARG_A(i);
  int n = GETARG_C(i);
  mrb_callinfo *ci;
  mrb_value recv;
  mrb_sym mid = syms[GETARG_B(i)];

  recv = regs[a];

  //printf("%d %x %x %x %x %x\n", MRB_PROC_CFUNC_P(m), irep->iseq, pc,regs[a].ttt, regs, n);
  //puts(mrb_sym2name(mrb, mid));

  /* push callinfo */
  ci = mrbjit_cipush(mrb);
  ci->mid = mid;
  ci->proc = m;
  ci->stackidx = mrb->stack - mrb->stbase;
  ci->argc = n;
  ci->target_class = c;
  ci->pc = pc + 1;
  ci->acc = a;

  /* prepare stack */
  mrb->stack += a;

  /* setup environment for calling method */
  *status->proc = mrb->ci->proc = m;
  irep = *status->irep = m->body.irep;
  *status->pool = irep->pool;
  *status->syms = irep->syms;
  ci->nregs = irep->nregs;
  mrbjit_stack_extend(mrb, irep->nregs,  ci->argc+2);
  *status->regs = mrb->stack;
  *status->pc = irep->iseq;
  //mrb_p(mrb, recv);

  return NULL;
}

void *
mrbjit_exec_enter(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = *status->regs;
  mrb_code i = *pc;

  //printf("enter %x %x \n", pc, mrb->ci);
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

  //printf("rc %x %x %s\n", *status->pc, mrb->ci, mrb_sym2name(mrb, mrb->ci->mid));
  return NULL;
}

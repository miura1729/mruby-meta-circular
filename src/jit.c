/*
** jit.c - Toplevel of JIT
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "opcode.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "mruby/proc.h"
#include "mruby/class.h"
#include "mruby/array.h"
#include <stddef.h>
#include <stdio.h>

#define ISEQ_OFFSET_OF(pc) ((size_t)((pc) - irep->iseq))

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

static mrbjit_code_info *
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

static mrbjit_code_info *
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

  if (ci) {
    if (cbase) {
      if (ci->entry) {
	mrbjit_gen_jump_block(cbase, ci->entry);
      }
      else {
	mrbjit_gen_exit(cbase, mrb, irep, ppc);
      }
	
      /* Finish compile */
      mrb->compile_info.code_base = NULL;
    }

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
      //printf("%x \n", *ppc);

      irep = *status->irep;
      regs = *status->regs;
      if (rc) {
	mrb->compile_info.prev_pc = *ppc;
	return rc;
      }
      //      printf("%x %x \n", ci->entry, regs);
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

void
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
    //    if (mrb->exc) goto L_RAISE;
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
}

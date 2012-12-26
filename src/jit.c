/*
** jit.c - Toplevel of JIT
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/array.h"
#include <stddef.h>
#include <stdio.h>

#define ISEQ_OFFSET_OF(pc) ((size_t)((pc) - irep->iseq))

extern const void *mrbjit_emit_code(mrb_state *, mrb_irep *, mrb_code **);
extern void mrbjit_emit_exit(mrbjit_code_area, mrb_state *, mrb_irep *, mrb_code **);

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
    tab->body = mrb_malloc(mrb, sizeof(mrbjit_code_info) * tab->size);
    for (i = oldsize; i < tab->size; i++) {
      tab->body[i].entry = NULL;
    }
  }

  oldsize = tab->size;
  for (i = 0; i < tab->size; i++) {
    ele = tab->body + i;
    if (ele->entry == NULL) {
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

void
mrbjit_dispatch(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  size_t n = ISEQ_OFFSET_OF(*ppc);
  mrbjit_code_info *ci;
  mrbjit_code_area cbase;
  mrb_code *prev_pc;

  prev_pc = irep->compile_info->prev_pc;
  cbase = irep->compile_info->code_base;
  if (prev_pc) {
    ci = search_codeinfo_prev(irep->jit_entry_tab + n, prev_pc);
  }
  else {
    ci = NULL;
  }

  if (ci) {
    if (cbase == NULL) {
      mrbjit_emit_exit(ci->code_base, mrb, irep, ppc);
      /* Finish compile */
      irep->compile_info->code_base = NULL;
    }

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

    ci->entry();

    asm("pop %ebx");
    asm("pop %ecx");
  }
  else {
    if (irep->prof_info[n]++ > COMPILE_THRESHOLD) {
      void *(*entry)();

      entry = mrbjit_emit_code(mrb, irep, ppc);
      if (entry) {
	ci = add_codeinfo(mrb, irep->jit_entry_tab + n);
	ci->code_base = irep->compile_info->code_base;
	ci->prev_pc = prev_pc;
	ci->entry = entry;
      }
    }
    if (cbase && ci == NULL) {
      mrbjit_code_info *pci;
      size_t pn;
      
      /* Finish compile */
      pn = ISEQ_OFFSET_OF(irep->compile_info->prev_pc);
      pci = search_codeinfo_cbase(irep->jit_entry_tab + pn, cbase);
      if (pci) {
	mrbjit_emit_exit(pci->code_base, mrb, irep, ppc);
	irep->compile_info->code_base = NULL;
      }
    }
    irep->compile_info->prev_pc = *ppc;
  }
}

void
mrbjit_dispatch_local_jump(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  mrbjit_dispatch(mrb, irep, ppc, regs);
  
}

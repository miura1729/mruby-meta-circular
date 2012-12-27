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

void
mrbjit_dispatch(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  size_t n;
  mrbjit_code_info *ci;
  mrbjit_code_area cbase;
  mrb_code *prev_pc;

 retry:
  n = ISEQ_OFFSET_OF(*ppc);
  prev_pc = irep->compile_info->prev_pc;
  if (prev_pc) {
    ci = search_codeinfo_prev(irep->jit_entry_tab + n, prev_pc);
  }
  else {
    ci = NULL;
  }

  cbase = irep->compile_info->code_base;
  if (ci) {
    if (cbase) {
      mrbjit_emit_exit(cbase, mrb, irep, ppc);
      /* Finish compile */
      irep->compile_info->code_base = NULL;
    }

    if (ci->entry) {
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

      ci->entry();

      asm("pop %ebx");
      asm("pop %ecx");
      //printf("%x \n", *ppc);

      /* Updated pc, so dispatch try again */
      goto retry;
    }
  }
  else {
    void *(*entry)() = NULL;

    if (irep->prof_info[n]++ > COMPILE_THRESHOLD) {
      entry = mrbjit_emit_code(mrb, irep, ppc);
      //      printf("size %x %x %x\n", irep->jit_entry_tab[n].size, *ppc, prev_pc);
      ci = add_codeinfo(mrb, irep->jit_entry_tab + n);
      ci->code_base = irep->compile_info->code_base;
      ci->prev_pc = prev_pc;
      ci->used = 1;
      ci->entry = entry;
    }

    if (cbase && entry == NULL) {
      /* Finish compile */
      mrbjit_emit_exit(cbase, mrb, irep, ppc);
      irep->compile_info->code_base = NULL;
    }
  }
  irep->compile_info->prev_pc = *ppc;
}

void
mrbjit_dispatch_local_jump(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  mrbjit_dispatch(mrb, irep, ppc, regs);
  
}

/*
** mruby/irep.h - mrb_irep structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_IREP_H
#define MRUBY_IREP_H

#include "jit.h"
#include <setjmp.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct mrb_irep {
  uint32_t idx;
  uint16_t nlocals;
  uint16_t nregs;
  uint8_t flags;

  mrb_code *iseq;
  mrb_value *pool;
  mrb_sym *syms;

  /* debug info */
  const char *filename;
  uint16_t *lines;

  size_t ilen, plen, slen;

  mrb_int is_method_cache_used;

  /* Lambda optimize */
  int simple_lambda;
  int shared_lambda;
  struct RProc *proc_obj;

  /* JIT stuff */
  int *prof_info;
  int jit_inlinep;
  mrbjit_codetab *jit_entry_tab;
  void *(*jit_top_entry)();
} mrb_irep;

typedef struct mrbjit_vmstatus {
  mrb_irep **irep;
  struct RProc **proc;
  mrb_code **pc;
  mrb_value **pool;
  mrb_sym **syms;
  mrb_value **regs;
  int *ai;
  void **optable;
  void **gototable;
  jmp_buf **prev_jmp;
} mrbjit_vmstatus;

#define MRB_ISEQ_NO_FREE 1

mrb_irep *mrb_add_irep(mrb_state *mrb);
mrb_value mrb_load_irep(mrb_state*, const uint8_t*);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_IREP_H */

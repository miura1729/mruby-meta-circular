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

enum method_kind {
  NORMAL = 0,
  IV_READER,
  IV_WRITER,
};

/* Program data array struct */
typedef struct mrb_irep {
  uint16_t nlocals;        /* Number of local variables */
  uint16_t nregs;          /* Number of register variables */
  uint8_t flags;

  mrb_code *iseq;
  struct irep_pool {
    union {
      mrb_float f;
      struct irep_pool_string {
        mrb_int len;
        char buf[1];
      } *s;
      mrb_int i;
    } value;
    enum mrb_vtype type;
  } *pool;
  mrb_sym *syms;
  struct mrb_irep **reps;

  /* debug info */
  const char *filename;
  uint16_t *lines;
  struct mrb_irep_debug_info* debug_info;

  size_t ilen, plen, slen, rlen, refcnt;

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
  enum method_kind method_kind;
} mrb_irep;

typedef struct mrbjit_vmstatus {
  mrb_irep **irep;
  struct RProc **proc;
  mrb_code **pc;
  struct irep_pool **pool;
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
void mrb_irep_free(mrb_state*, struct mrb_irep*);
void mrb_irep_incref(mrb_state*, struct mrb_irep*);
void mrb_irep_decref(mrb_state*, struct mrb_irep*);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_IREP_H */

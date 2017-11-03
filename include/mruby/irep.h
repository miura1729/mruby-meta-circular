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

#include "common.h"
#include <mruby/compile.h>

/**
 * Compiled mruby scripts.
 */
MRB_BEGIN_DECL

enum irep_pool_type {
  IREP_TT_STRING,
  IREP_TT_FIXNUM,
  IREP_TT_FLOAT,
  IREP_TT_FALSE,
};

struct mrb_locals {
  mrb_sym name;
  uint16_t r;
};

/* Program data array struct */
typedef struct mrb_irep {
  uint32_t nlocals;        /* Number of local variables */
  uint32_t nregs;          /* Number of register variables */
  uint32_t flags;

  mrb_code *iseq;
  mrb_value *pool;
  mrb_sym *syms;
  struct mrb_irep **reps;

  struct mrb_locals *lv;
  /* debug info */
  mrb_bool own_filename;
  const char *filename;
  uint16_t *lines;
  struct mrb_irep_debug_info* debug_info;

  size_t ilen, plen, slen, rlen, refcnt;

  mrb_int is_method_cache_used;

  /* Lambda optimize */
  signed char simple_lambda;
  signed char shared_lambda;
  signed char block_lambda;
  signed char may_overflow;
  signed char disable_jit;
  struct RProc *proc_obj;

  /* JIT stuff */
  int *prof_info;
  uint32_t jit_inlinep;
  uint16_t arg_ver_num;
  mrbjit_codetab *jit_entry_tab;
  enum method_kind method_kind;
  const void *entry;
  struct RProc *outer;      /* Refers outer scope */
} mrb_irep;

typedef struct mrbjit_vmstatus {
  mrb_irep **irep;
  struct RProc **proc;
  mrb_code **pc;
  mrb_value **pool;
  mrb_sym **syms;
  int *ai;
  struct mrbjit_vmstatus *status;
  void **optable;
  void **gototable;
  struct mrb_jmpbuf **prev_jmp;
} mrbjit_vmstatus;

#define MRB_ISEQ_NO_FREE 1

MRB_API mrb_irep *mrb_add_irep(mrb_state *mrb);
MRB_API mrb_value mrb_load_irep(mrb_state*, const uint8_t*);
MRB_API mrb_value mrb_load_irep_cxt(mrb_state*, const uint8_t*, mrbc_context*);
MRB_API void mrb_irep_free(mrb_state*, struct mrb_irep*);
MRB_API void mrb_irep_incref(mrb_state*, struct mrb_irep*);
MRB_API void mrb_irep_decref(mrb_state*, struct mrb_irep*);
void mrbjit_make_jit_entry_tab(mrb_state *, mrb_irep *, int);

MRB_END_DECL

#endif  /* MRUBY_IREP_H */

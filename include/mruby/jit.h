/*
** mruby/jit.h - JIT structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JIT_H
#define MRUBY_JIT_H

#define COMPILE_THRESHOLD 1000

typedef struct mrbjit_varinfo {
  /* SRC */
  int reg_no;
  int up;

  /* DIST */
  enum {
    REG,
    MEMORY,
    STACK_FRMAME
  } where;
  union {
    void *ptr;
    int no;
    int offset;
  } addr;
} mrbjit_varinfo;

typedef union mrbjit_inst_spec_info {
  mrbjit_varinfo varinfo;	/* For Local assignment */
  mrb_code *branch_inst;	/* For conditional brnach */
} mrbjit_inst_spec_info;

typedef void * mrbjit_code_area;

typedef struct mrbjit_code_info {
  mrbjit_code_area code_base;
  mrb_code *prev_pc;
  void *(*entry)();
  mrbjit_inst_spec_info inst_spec;
} mrbjit_code_info;

typedef struct mrbjit_codetab {
  int size;
  mrbjit_code_info *body;
} mrbjit_codetab;

typedef struct mrbjit_comp_info {
  mrb_code *prev_pc;
  void *(*code_base)();
} mrbjit_comp_info;

#endif  /* MRUBY_JIT_H */

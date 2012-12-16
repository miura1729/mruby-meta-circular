/*
** mruby/jit.h - JIT structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JIT_H
#define MRUBY_JIT_H

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

typedef struct mrbjit_code_info {
  void *(*code_base)();
  mrb_code *prev_pc;
  void *(*entry)();
  mrbjit_varinfo varinfo;
} mrbjit_code_info;

typedef struct mrbjit_codetab {
  int size;
  mrbjit_code_info *codeinfo;
} mrbjit_codetab;

typedef struct mrbjit_comp_info {
  mrb_code *prev_pc;
} mrbjit_comp_info;

#endif  /* MRUBY_JIT_H */

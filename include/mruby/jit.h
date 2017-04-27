/*
** mruby/jit.h - JIT structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JIT_H
#define MRUBY_JIT_H

typedef struct mrbjit_vmstatus mrbjit_vmstatus;

#define COMPILE_THRESHOLD 10
#define NO_INLINE_METHOD_LEN 10

typedef struct mrbjit_codetab {
  int size;
  mrbjit_code_info *body;
} mrbjit_codetab;

typedef enum {
  LOCALJUMP_ERROR_RETURN = 0,
  LOCALJUMP_ERROR_BREAK = 1,
  LOCALJUMP_ERROR_YIELD = 2
} localjump_error_kind;

int mrbjit_check_inlineble(mrb_state *, struct mrb_irep *);

mrb_value mrb_uvget(mrb_state *, int, int);
void mrb_uvset(mrb_state *, int, int, mrb_value);
mrb_callinfo* mrbjit_cipush(mrb_state *);
void mrbjit_cipop(mrb_state *);
void mrbjit_stack_extend(mrb_state *, int);
void mrbjit_argnum_error(mrb_state *, int);
void mrbjit_ecall(mrb_state *, int);
struct REnv* mrbjit_top_env(mrb_state *, struct RProc *);
void mrbjit_localjump_error(mrb_state *, localjump_error_kind);

extern void mrbjit_gen_exit_patch(mrbjit_code_area, mrb_state *, void *, mrb_code *, mrbjit_vmstatus *, mrbjit_code_info *);
extern void mrbjit_gen_exit_patch2(mrbjit_code_area, mrb_state *, void *, mrb_code *, mrbjit_vmstatus *, mrbjit_code_info *);

mrbjit_code_info *mrbjit_search_codeinfo_prev(mrbjit_codetab *, mrb_code *, mrb_code *, uint16_t);

void disp_type(mrb_state *, mrbjit_reginfo *rinfo);
void mrbjit_reset_proc(mrb_state *, mrbjit_vmstatus *, struct RProc *);
void mrbjit_reset_caller();
mrb_value mrbjit_instance_alloc(mrb_state *, mrb_value);

#define ISEQ_OFFSET_OF(pc) ((size_t)((pc) - irep->iseq))

#endif  /* MRUBY_JIT_H */

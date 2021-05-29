#include "mruby/jitcode.h"
extern "C" {
#include "mruby.h"
#include "mruby/primitive.h"
#include "mruby/array.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "mruby/opcode.h"
}

#define PVEC4_ALLOCATE                                               \
do {                                                                 \
   /* TODO add guard of class  */                                    \
                                                                     \
  inLocalLabel();                                                    \
   /* obj = mrbjit_instance_alloc(mrb, klass); */                    \
  if (civoff >= 0) {                                                 \
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv)); \
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);                      \
    emit_push(mrb, coi, reg_tmp0);			/* PUSH __objcache__ */ \
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, civoff * sizeof(mrb_value) + 4); \
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, civoff * sizeof(mrb_value)); \
    test(reg_tmp0, reg_tmp0);                                                  \
    jz(".empty");                                                    \
    push(reg_tmp0);                                                       \
    push(reg_tmp1);                                                       \
    emit_add(mrb, coi, reg_tmp0, reg_mrb);                           \
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, OffsetOf(struct RObject, c));  \
    test(reg_tmp1, reg_tmp1);                                        \
    jz(".setnil");                                                   \
    emit_move(mrb, coi, reg_tmp0, reg_sp, 8);                           \
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1); \
                                                                     \
    emit_cfunc_start(mrb, coi);                                      \
    emit_load_literal(mrb, coi, reg_tmp1, (cpu_word_t)c);            \
    emit_arg_push(mrb, coi, 1, reg_tmp1);                            \
    emit_arg_push(mrb, coi, 0, reg_mrb);                                 \
    emit_call(mrb, coi, (void *)mrb_write_barrier); 		     \
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *)); \
    jmp(".ee");                                                      \
    L(".setnil");                                                    \
    emit_move(mrb, coi, reg_tmp0, reg_sp, 8);                           \
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1); \
    emit_load_literal(mrb, coi, reg_tmp1, 0xfff00000 | MRB_TT_FALSE); \
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value) + 4, reg_tmp1); \
    L(".ee");                                                        \
    pop(reg_tmp1);                                                        \
    pop(reg_tmp0);                                                        \
    jmp("@f");                                                       \
    L(".empty");                                                     \
  }                                                                  \
                                                                     \
  emit_cfunc_start(mrb, coi);                                        \
  emit_load_literal(mrb, coi, reg_tmp0, 4);                          \
  emit_arg_push(mrb, coi, 1, reg_tmp0);                              \
  emit_arg_push(mrb, coi, 0, reg_mrb);                               \
  emit_call(mrb, coi, (void *)mrb_ary_new_capa);		     \
  emit_cfunc_end(mrb, coi, 2 * sizeof(void *));                      \
                                                                     \
  L("@@");                                                           \
 } while(0)

#define PVEC4_ALLOCATE_FIN                                           \
do {                                                                 \
  if (civoff >= 0) {                                                 \
    emit_pop(mrb, coi, reg_tmp0);			/* POP __objcache__ */ \
  }                                                                  \
                                                                     \
  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);              \
  emit_move(mrb, coi, reg_tmp0, OffsetOf(struct RArray, c), (cpu_word_t)c); \
  emit_load_literal(mrb, coi, reg_tmp1, 4);                          \
  emit_move(mrb, coi, reg_tmp0, OffsetOf(struct RArray, as.heap.len), reg_tmp1); \
  outLocalLabel();                                                   \
                                                                     \
  dinfo->type = MRB_TT_ARRAY;                                        \
  dinfo->klass = c;                                                  \
  dinfo->constp = 0;                                                 \
} while(0)

mrb_value
MRBJitCode::mrbjit_prim_pvec4_new_impl(mrb_state *mrb, mrb_value proc,
				       mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  struct RClass *c = mrb_class_ptr(regs[a]);
  mrb_value klass = regs[a];
  mrbjit_reginfo *dinfo = &coi->reginfo[a];
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  dinfo->unboxedp = 0;

  gen_flush_regs(mrb, pc, status, coi, 1);

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  PVEC4_ALLOCATE;

  // regs[a] = obj;
  /* reg_tmp0 store pointer to new vector */
  emit_local_var_write_from_cfunc(mrb, coi, a);
  emit_add(mrb, coi, reg_tmp0, reg_mrb);

  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RArray, as.heap.ptr));

  movupd(xmm0, ptr [reg_regs + (a + 1) * sizeof(mrb_value)]);
  movupd(ptr [reg_tmp0], xmm0);
  movupd(xmm0, ptr [reg_regs + (a + 3) * sizeof(mrb_value)]);
  movupd(ptr [reg_tmp0 + 16], reg_dtmp0);

  PVEC4_ALLOCATE_FIN;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pvec4_new(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_new_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_pvec4_add_impl(mrb_state *mrb, mrb_value proc,
				       mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  struct RClass *c = mrb_obj_ptr(regs[a])->c;
  mrb_value klass = mrb_obj_value(c);
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  dinfo->unboxedp = 0;

  gen_flush_regs(mrb, pc, status, coi, 1);

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, c));
  PVEC4_ALLOCATE;

  // regs[a] = obj;
  /* reg_tmp0 store pointer to new vector */

  emit_push(mrb, coi, reg_vars);

  emit_local_var_ptr_value_read(mrb, coi, reg_vars, a);
  emit_move(mrb, coi, reg_vars, reg_vars, OffsetOf(struct RArray, as.heap.ptr));

  emit_local_var_value_write(mrb, coi, a, reg_tmp0s);
  emit_add(mrb, coi, reg_tmp0, reg_mrb);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RArray, as.heap.ptr));

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, a + 1);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, as.heap.ptr));

  movupd(xmm0, ptr [reg_vars]);
  movupd(xmm1, ptr [reg_tmp1]);
  addpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0], xmm0);

  movupd(xmm0, ptr [reg_vars + 16]);
  movupd(xmm1, ptr [reg_tmp1 + 16]);
  addpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0 + 16], xmm0);

  emit_pop(mrb, coi, reg_vars);

  PVEC4_ALLOCATE_FIN;


  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pvec4_add(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_add_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_pvec4_sub_impl(mrb_state *mrb, mrb_value proc,
				       mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  struct RClass *c = mrb_obj_ptr(regs[a])->c;
  mrb_value klass = mrb_obj_value(c);
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  dinfo->unboxedp = 0;

  gen_flush_regs(mrb, pc, status, coi, 1);

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, c));
  PVEC4_ALLOCATE;

  // regs[a] = obj;
  /* reg_tmp0 store pointer to new vector */

  emit_push(mrb, coi, reg_vars);

  emit_local_var_ptr_value_read(mrb, coi, reg_vars, a);
  emit_move(mrb, coi, reg_vars, reg_vars, OffsetOf(struct RArray, as.heap.ptr));

  emit_local_var_value_write(mrb, coi, a, reg_tmp0s);
  emit_add(mrb, coi, reg_tmp0, reg_mrb);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RArray, as.heap.ptr));

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, a + 1);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, as.heap.ptr));

  movupd(xmm0, ptr [reg_vars]);
  movupd(xmm1, ptr [reg_tmp1]);
  subpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0], xmm0);

  movupd(xmm0, ptr [reg_vars + 16]);
  movupd(xmm1, ptr [reg_tmp1 + 16]);
  subpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0 + 16], xmm0);

  emit_pop(mrb, coi, reg_vars);

  PVEC4_ALLOCATE_FIN;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pvec4_sub(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_sub_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_pvec4_aget_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_code i = *pc;
  int regno = GETARG_A(i);
  const cpu_word_t aryno = regno;
  const cpu_word_t idxno = aryno + 1;
  mrbjit_reginfo *ainfo = &coi->reginfo[regno];
  mrbjit_reginfo *iinfo = &coi->reginfo[regno + 1];

  if (iinfo->regplace == MRBJIT_REG_IMMIDATE) {
    emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, as.heap.ptr));
    movsd(xmm0, ptr [reg_tmp1 + mrb_fixnum(iinfo->value) * sizeof(mrb_value)]);
    emit_local_var_write(mrb, coi, aryno, xmm0);
  }
  else {
    gen_flush_regs(mrb, pc, status, coi, 1);

    emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, as.heap.ptr));
    emit_local_var_value_read(mrb, coi, reg_tmp0s, idxno);
    movsd(xmm0, ptr [reg_tmp1 + reg_tmp0 * sizeof(mrb_value)]);
    emit_local_var_write(mrb, coi, aryno, xmm0);
  }
  
  ainfo->unboxedp = 0;
  ainfo->regplace = MRBJIT_REG_MEMORY;
  ainfo->type = MRB_TT_FLOAT;
  ainfo->klass = mrb->float_class;
  ainfo->constp = 0;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pvec4_aget(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_aget_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_pvec4_aset_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int regno = GETARG_A(i);
  const cpu_word_t aryno = regno;
  const cpu_word_t idxno = aryno + 1;
  const cpu_word_t valno = idxno + 1;
  mrbjit_reginfo *iinfo = &coi->reginfo[regno + 1];

  if (iinfo->regplace == MRBJIT_REG_IMMIDATE) {
    emit_local_var_read(mrb, coi, xmm0, valno);
    emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, as.heap.ptr));
    movsd(ptr [reg_tmp1 + mrb_fixnum(iinfo->value) * sizeof(mrb_value)], xmm0);
  }
  else {
    gen_flush_regs(mrb, pc, status, coi, 1);
    emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
    emit_local_var_value_read(mrb, coi, reg_tmp0s, idxno);

    emit_local_var_read(mrb, coi, xmm0, valno);
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, as.heap.ptr));
    movsd(ptr [reg_tmp1 + reg_tmp0 * sizeof(mrb_value)], xmm0);
  }

  /*  emit_cfunc_start(mrb, coi);

  emit_local_var_type_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 3, reg_tmp0);
  emit_local_var_value_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 2, reg_tmp0);
  emit_local_var_value_read(mrb, coi, reg_tmp1, aryno);
  emit_arg_push(mrb, coi, 1, reg_tmp1);
  emit_arg_push(mrb, coi, 0, esi);
  emit_call(mrb, coi, (void *)mrb_field_write_barrier);
  emit_cfunc_end(mrb, coi, 2 * sizeof(void *) + sizeof(mrb_value));*/

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pvec4_aset(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_aset_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}



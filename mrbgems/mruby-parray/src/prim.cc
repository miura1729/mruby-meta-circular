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
    emit_local_var_value_read(mrb, coi, reg_tmp0, a);                \
    emit_move(mrb, coi, reg_tmp0, eax, OffsetOf(struct RObject, c)); \
    emit_move(mrb, coi, reg_tmp0, eax, OffsetOf(struct RObject, iv)); \
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);                      \
    emit_push(mrb, coi, eax);			/* PUSH __objcache__ */ \
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, civoff * sizeof(mrb_value) + 4); \
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, civoff * sizeof(mrb_value)); \
    test(eax, eax);                                                  \
    jz(".empty");                                                    \
    push(eax);                                                       \
    push(edx);                                                       \
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, OffsetOf(struct RObject, c));  \
    test(edx, edx);                                                  \
    jz(".setnil");                                                   \
    push(edx);                                                       \
    emit_cfunc_start(mrb, coi);                                      \
    emit_arg_push(mrb, coi, 1, reg_tmp0);                            \
    emit_arg_push(mrb, coi, 0, esi);                                 \
    call((void *)mrb_write_barrier);                                 \
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *)); \
    pop(edx);                                                        \
                                                                     \
    emit_move(mrb, coi, reg_tmp0, esp, 8);                           \
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1); \
    jmp(".ee");                                                      \
    L(".setnil");                                                    \
    emit_move(mrb, coi, reg_tmp0, esp, 8);                           \
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1); \
    emit_load_literal(mrb, coi, reg_tmp1, 0xfff00000 | MRB_TT_FALSE); \
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value) + 4, reg_tmp1); \
    L(".ee");                                                         \
    pop(edx);                                                         \
    pop(eax);                                                         \
    jmp("@f");                                                        \
    L(".empty");                                                      \
  }                                                                   \
                                                                      \
  emit_cfunc_start(mrb, coi);                                         \
  emit_load_literal(mrb, coi, reg_tmp0, 4);                           \
  emit_arg_push(mrb, coi, 1, reg_tmp0);                               \
  emit_arg_push(mrb, coi, 0, esi);                                    \
  call((void *)mrb_ary_new_capa);                                     \
  emit_cfunc_end(mrb, coi, 2 * sizeof(void *));                       \
                                                                      \
  L("@@");                                                            \
 } while(0)

#define PVEC4_ALLOCATE_FIN                                            \
do {                                                                  \
  if (civoff >= 0) {                                                  \
    emit_pop(mrb, coi, eax);			/* POP __objcache__ */ \
  }                                                                   \
                                                                      \
  emit_local_var_value_read(mrb, coi, reg_tmp0, a);                   \
  emit_move(mrb, coi, reg_tmp0, OffsetOf(struct RArray, c), (Xbyak::uint32)c); \
  outLocalLabel();                                                    \
                                                                      \
  dinfo->type = MRB_TT_ARRAY;                                         \
  dinfo->klass = c;                                                   \
  dinfo->constp = 0;                                                  \
} while(0)

mrb_value
MRBJitCode::mrbjit_prim_pvec4_add_impl(mrb_state *mrb, mrb_value proc,
				       mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
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

  PVEC4_ALLOCATE;

  // regs[a] = obj;
  /* reg_tmp0 store pointer to new vector */

  emit_push(mrb, coi, ebx);

  emit_local_var_value_read(mrb, coi, ebx, a);
  emit_move(mrb, coi, ebx, ebx, OffsetOf(struct RArray, ptr));

  emit_local_var_value_write(mrb, coi, a, reg_tmp0);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RArray, ptr));

  emit_local_var_value_read(mrb, coi, reg_tmp1, a + 1);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, ptr));

  movupd(xmm0, ptr [ebx]);
  movupd(xmm1, ptr [reg_tmp1]);
  addpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0], xmm0);

  movupd(xmm0, ptr [ebx + 16]);
  movupd(xmm1, ptr [reg_tmp1 + 16]);
  addpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0 + 16], xmm0);

  emit_pop(mrb, coi, ebx);

  PVEC4_ALLOCATE_FIN;


  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pve4_add(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_add_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_pvec4_sub_impl(mrb_state *mrb, mrb_value proc,
				       mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
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

  PVEC4_ALLOCATE;

  // regs[a] = obj;
  /* reg_tmp0 store pointer to new vector */

  emit_push(mrb, coi, ebx);

  emit_local_var_value_read(mrb, coi, ebx, a);
  emit_move(mrb, coi, ebx, ebx, OffsetOf(struct RArray, ptr));

  emit_local_var_value_write(mrb, coi, a, reg_tmp0);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RArray, ptr));

  emit_local_var_value_read(mrb, coi, reg_tmp1, a + 1);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, ptr));

  movupd(xmm0, ptr [ebx]);
  movupd(xmm1, ptr [reg_tmp1]);
  subpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0], xmm0);

  movupd(xmm0, ptr [ebx + 16]);
  movupd(xmm1, ptr [reg_tmp1 + 16]);
  subpd(xmm0, xmm1);
  movupd(ptr [reg_tmp0 + 16], xmm0);

  emit_pop(mrb, coi, ebx);

  PVEC4_ALLOCATE_FIN;


  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pve4_sub(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_sub_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}


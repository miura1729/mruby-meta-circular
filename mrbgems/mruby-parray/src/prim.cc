#include "mruby/jitcode.h"
extern "C" {
#include "mruby.h"
#include "mruby/primitive.h"
#include "mruby/array.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "mruby/opcode.h"

mrb_value
mrbjit_instance_alloc_pa(mrb_state *mrb, mrb_value cv)
{
  struct RClass *c = mrb_obj_ptr(cv)->c;
  struct RObject *o;
  enum mrb_vtype ttype = MRB_INSTANCE_TT(c);
  segment *seg;

  if (c->tt == MRB_TT_SCLASS)
    mrb_raise(mrb, E_TYPE_ERROR, "can't create instance of singleton class");

  if (ttype == 0) ttype = MRB_TT_OBJECT;
  o = (struct RObject*)mrb_obj_alloc(mrb, ttype, c);
  o->iv = (iv_tbl *)mrb_malloc(mrb, sizeof(iv_tbl));
  o->iv->last_len = 0;
  seg = (segment *)mrb_malloc(mrb, sizeof(segment));
  o->iv->rootseg = seg;

  return mrb_obj_value(o);
}
}

mrb_value
MRBJitCode::mrbjit_prim_pvec4_add_impl(mrb_state *mrb, mrb_value proc,
				       mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  mrb_value klass = regs[a];
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  dinfo->unboxedp = 0;

  gen_flush_regs(mrb, pc, status, coi, 1);

  // TODO add guard of class
  
  // obj = mrbjit_instance_alloc(mrb, klass);
  if (civoff >= 0) {
    emit_local_var_value_read(mrb, coi, reg_tmp0, a);
    emit_move(mrb, coi, reg_tmp0, eax, OffsetOf(struct RObject, iv));
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
    emit_push(mrb, coi, eax);			/* PUSH __objcache__ */
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, civoff * sizeof(mrb_value) + 4);
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, civoff * sizeof(mrb_value));
    test(eax, eax);
    jnz("@f");
  }    

  emit_cfunc_start(mrb, coi);
  emit_load_literal(mrb, coi, reg_tmp0, *((Xbyak::uint32 *)(&klass) + 1));
  emit_arg_push(mrb, coi, 2, eax);
  emit_load_literal(mrb, coi, reg_tmp0, *((Xbyak::uint32 *)(&klass)));
  emit_arg_push(mrb, coi, 1, eax);
  emit_arg_push(mrb, coi, 0, esi);
  call((void *)mrbjit_instance_alloc_pa);
  emit_cfunc_end(mrb, coi, 3 * sizeof(void *));

  L("@@");
  // regs[a] = obj;
  /* reg_tmp0 store pointer to new vector */

  emit_push(mrb, coi, reg_tmp0);
  
  emit_local_var_value_read(mrb, coi, reg_tmp1, a + 1);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RObject, iv));
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, 0);
  emit_local_var_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);

  movupd(xmm0, ptr [eax]);
  paddq(xmm0, ptr [edx]);
  movupd(xmm1, ptr [eax + 16]);
  paddq(xmm1, ptr [edx + 16]);

  emit_pop(mrb, coi, reg_tmp0);
  emit_local_var_value_write(mrb, coi, a, reg_tmp0);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);

  movupd(ptr [eax], xmm0);
  movupd(ptr [eax + 16], xmm1);

  /*  emit_move(mrb, coi, reg_tmp0, eax, OffsetOf(struct RObject, iv));
  emit_load_literal(mrb, coi, reg_tmp1, 0);
  emit_move(mrb, coi, reg_tmp0, OffsetOf(iv_tbl, last_len), reg_tmp1);*/

  if (civoff >= 0) {
    emit_pop(mrb, coi, eax);			/* POP __objcache__ */ 
    emit_load_literal(mrb, coi, reg_tmp1, 0);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1);
    emit_load_literal(mrb, coi, reg_tmp1, 0xfff00000 | MRB_TT_FALSE);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value) + 4, reg_tmp1);
  }    

  dinfo->type = MRB_TT_OBJECT;
  dinfo->klass = mrb_class_ptr(klass);
  dinfo->constp = 0;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_pve4_add(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_pvec4_add_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}


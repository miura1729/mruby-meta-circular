#include "mruby/jitcode.h"
#include <math.h>
extern "C" {
#include <mruby.h>
#include <mruby/primitive.h>
#include <mruby/array.h>
#include <mruby/irep.h>
#include <mruby/variable.h>
#include <mruby/opcode.h>

mrb_value
mrbjit_instance_alloc(mrb_state *mrb, mrb_value cv)
{
  struct RClass *c = mrb_class_ptr(cv);
  struct RObject *o;
  enum mrb_vtype ttype = MRB_INSTANCE_TT(c);

  if (c->tt == MRB_TT_SCLASS)
    mrb_raise(mrb, E_TYPE_ERROR, "can't create instance of singleton class");

  if (ttype == 0) ttype = MRB_TT_OBJECT;
  o = (struct RObject*)mrb_obj_alloc(mrb, ttype, c);
  if (ttype == MRB_TT_OBJECT) {
    o->iv = &o->ivent;
  }
  return mrb_obj_value(o);
}

}

mrb_value
MRBJitCode::mrbjit_prim_num_cmp_impl(mrb_state *mrb, mrb_value proc,
				     mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;
  // not need guard for self because guard geneate already
  //local_var_type_read(reg_tmp0, regno);
  //gen_type_guard(mrb, (enum mrb_vtype)mrb_type(regs[regno]), pc);

  if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&
      mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {
    gen_type_guard(mrb, regno, status, pc, coi);
    gen_type_guard(mrb, regno + 1, status, pc, coi);

    emit_local_var_read(mrb, coi, reg_dtmp0, regno);
    emit_local_var_int_value_read(mrb, coi, reg_dtmp1, regno + 1);
    emit_cmp(mrb, coi, reg_dtmp0, reg_dtmp1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, regno, status, pc, coi);
    gen_type_guard(mrb, regno + 1, status, pc, coi);

    emit_local_var_int_value_read(mrb, coi, reg_dtmp0, regno);
    emit_local_var_read(mrb, coi, reg_dtmp1, regno + 1);
    emit_cmp(mrb, coi, reg_dtmp0, reg_dtmp1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, regno, status, pc, coi);
    gen_type_guard(mrb, regno + 1, status, pc, coi);

    emit_local_var_read(mrb, coi, reg_dtmp0, regno);
    emit_local_var_read(mrb, coi, reg_dtmp1, regno + 1);
    emit_cmp(mrb, coi, reg_dtmp0, reg_dtmp1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {
    emit_local_var_int_value_read(mrb, coi, reg_dtmp0, regno);
    emit_local_var_int_value_read(mrb, coi, reg_dtmp1, regno + 1);
    emit_cmp(mrb, coi, reg_dtmp0, reg_dtmp1);
    /*	  emit_local_var_value_read(mrb, coi, reg_tmp0, regno);
	  emit_cmp(mrb, coi, reg_tmp0, reg_regs, off1);*/
  }
  else {
    return mrb_nil_value();
  }

  inLocalLabel();
  jnz(".cmpneq");

  emit_load_literal(mrb, coi, reg_tmp0s, 0);
  emit_jmp(mrb, coi, ".cmpend");

  L(".cmpneq");
  jb(".cmplt");

  emit_load_literal(mrb, coi, reg_tmp0s, 1);
  emit_jmp(mrb, coi, ".cmpend");

  L(".cmplt");
  emit_load_literal(mrb, coi, reg_tmp0s, -1);

  L(".cmpend");
  outLocalLabel();

  emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);
  emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FIXNUM);
  emit_local_var_type_write(mrb, coi, regno, reg_tmp0s);
  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_num_cmp(mrb_state *mrb, mrb_value proc,
		    void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_num_cmp_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_succ_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  emit_local_var_add(mrb, coi, regno, 1);
  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_succ(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_succ_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_mod_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM) {
    return mrb_nil_value();
  }
  gen_flush_regs(mrb, pc, status, coi, 1);

  gen_type_guard(mrb, regno, status, pc, coi);
  gen_type_guard(mrb, regno + 1, status, pc, coi);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  emit_local_var_div(mrb, coi, regno + 1);
  test(reg_tmp1s, reg_tmp1s);
  jz("@f");
  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  xor(reg_tmp0s, dword [reg_regs + (regno + 1) * 8]);
  sar(reg_tmp0s, 31);
  and(reg_tmp0s, dword [reg_regs + (regno + 1) * 8]);
  emit_add(mrb, coi, reg_tmp1s, reg_tmp0s);
  L("@@");

  emit_local_var_value_write(mrb, coi, regno, reg_tmp1s);

  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_mod(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_mod_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_and_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM) {
    return mrb_nil_value();
  }
  gen_flush_regs(mrb, pc, status, coi, 1);

  gen_type_guard(mrb, regno, status, pc, coi);
  gen_type_guard(mrb, regno + 1, status, pc, coi);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  emit_local_var_value_read(mrb, coi, reg_tmp1s, regno + 1);
  and(reg_tmp0s, reg_tmp1s);
  emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);

  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_and(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_and_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_or_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM) {
    return mrb_nil_value();
  }
  gen_flush_regs(mrb, pc, status, coi, 1);

  gen_type_guard(mrb, regno, status, pc, coi);
  gen_type_guard(mrb, regno + 1, status, pc, coi);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  emit_local_var_value_read(mrb, coi, reg_tmp1s, regno + 1);
  or(reg_tmp0s, reg_tmp1s);
  emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);

  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_or(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_or_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_lshift_impl(mrb_state *mrb, mrb_value proc,
					mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM ||
      mrb_fixnum(regs[regno + 1]) < 0) {
    return mrb_nil_value();
  }
  gen_flush_regs(mrb, pc, status, coi, 1);

  gen_type_guard(mrb, regno, status, pc, coi);
  gen_type_guard(mrb, regno + 1, status, pc, coi);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  push(ecx);
  emit_local_var_value_read(mrb, coi, ecx, regno + 1);
  sal(reg_tmp0s, cl);
  pop(ecx);
  emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);

  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_lshift(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_lshift_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_rshift_impl(mrb_state *mrb, mrb_value proc,
					mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM ||
      mrb_fixnum(regs[regno + 1]) < 0) {
    return mrb_nil_value();
  }
  gen_flush_regs(mrb, pc, status, coi, 1);

  gen_type_guard(mrb, regno, status, pc, coi);
  gen_type_guard(mrb, regno + 1, status, pc, coi);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  push(ecx);
  emit_local_var_value_read(mrb, coi, ecx, regno + 1);
  sar(reg_tmp0s, cl);
  pop(ecx);
  emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);

  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_rshift(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_rshift_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fix_to_f_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  emit_local_var_int_value_read(mrb, coi, reg_dtmp0, regno);
  
  regno = get_dst_regno(mrb, status, coi, regno);
  dinfo = &coi->reginfo[regno];
  emit_local_var_write(mrb, coi, regno, reg_dtmp0);
  dinfo->type = MRB_TT_FLOAT;
  dinfo->klass = mrb->float_class;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_to_f(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_to_f_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_sym_cmp_impl(mrb_state *mrb, mrb_value proc,
					mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int regno = GETARG_A(i);
  int dstno = regno;
  mrbjit_reginfo *dinfo;

  gen_class_guard(mrb, dstno + 1, status, *status->pc, coi, mrb->symbol_class, 2);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
  emit_local_var_cmp(mrb, coi, reg_tmp0s, regno + 1);
  setnz(al);
  xor(ah, ah);
  cwde();

  dstno = get_dst_regno(mrb, status, coi, dstno);
  emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
  emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FIXNUM);
  emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);

  dinfo = &coi->reginfo[dstno];
  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  dinfo->unboxedp = 0;
  dinfo->constp = 0;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_sym_cmp(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_sym_cmp_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_obj_equal_m_impl(mrb_state *mrb, mrb_value proc,
					     mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code **ppc = status->pc;
  int regno = GETARG_A(**ppc);
  int dstno = get_dst_regno(mrb, status, coi, regno);
  mrb_value *regs  = mrb->c->stack;
  enum mrb_vtype tt = (enum mrb_vtype) mrb_type(regs[regno]);
  enum mrb_vtype tt2 = (enum mrb_vtype) mrb_type(regs[regno + 1]);
  mrbjit_reginfo *dinfo = &coi->reginfo[dstno];
  dinfo->unboxedp = 0;

  /* Import from class.h */
  switch (tt) {
  case MRB_TT_TRUE:
    gen_type_guard(mrb, regno, status, *ppc, coi);
    gen_type_guard(mrb, regno + 1, status, *ppc, coi);
    emit_load_literal(mrb, coi, reg_tmp0s, 1); /* true */
    COMP_BOOL_SET;
    break;

  case MRB_TT_FALSE:
  case MRB_TT_OBJECT:
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
  case MRB_TT_SCLASS:
    gen_type_guard(mrb, regno, status, *ppc, coi);
    gen_type_guard(mrb, regno + 1, status, *ppc, coi);
    emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
    emit_local_var_cmp(mrb, coi, reg_tmp0s, regno + 1);
    setz(al);
    COMP_BOOL_SET;
    break;

  default:
    return mrb_nil_value();
  }

  dinfo->type = MRB_TT_TRUE;
  dinfo->klass = mrb->true_class;
  dinfo->constp = 0;
  dinfo->unboxedp = 0;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_obj_equal_m(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_obj_equal_m_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_obj_not_equal_m_impl(mrb_state *mrb, mrb_value proc,
					     mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code **ppc = status->pc;
  int regno = GETARG_A(**ppc);
  int dstno = get_dst_regno(mrb, status, coi, regno);
  mrb_value *regs  = mrb->c->stack;
  enum mrb_vtype tt = (enum mrb_vtype) mrb_type(regs[regno]);
  enum mrb_vtype tt2 = (enum mrb_vtype) mrb_type(regs[regno + 1]);
  mrbjit_reginfo *dinfo = &coi->reginfo[dstno];
  dinfo->unboxedp = 0;

  /* Import from class.h */
  switch (tt) {
  case MRB_TT_TRUE:
    gen_type_guard(mrb, regno, status, *ppc, coi);
    gen_type_guard(mrb, regno + 1, status, *ppc, coi);
    emit_load_literal(mrb, coi, reg_tmp0s, 0); /* false */
    COMP_BOOL_SET;
    break;

  case MRB_TT_FLOAT:
    gen_type_guard(mrb, regno, status, *ppc, coi);
    gen_type_guard(mrb, regno + 1, status, *ppc, coi);
    emit_local_var_read(mrb, coi, reg_dtmp0, regno);
    emit_local_var_cmp(mrb, coi, reg_dtmp0, regno + 1);
    setnz(al);
    COMP_BOOL_SET;
    break;

  case MRB_TT_FALSE:
  case MRB_TT_FIXNUM:
  case MRB_TT_OBJECT:
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
  case MRB_TT_SCLASS:
    gen_type_guard(mrb, regno, status, *ppc, coi);
    gen_type_guard(mrb, regno + 1, status, *ppc, coi);
    emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);
    emit_local_var_cmp(mrb, coi, reg_tmp0s, regno + 1);
    setnz(al);
    COMP_BOOL_SET;
    break;

  default:
    return mrb_nil_value();
  }

  dinfo->type = MRB_TT_TRUE;
  dinfo->klass = mrb->true_class;
  dinfo->constp = 0;
  dinfo->unboxedp = 0;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_obj_not_equal_m(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_obj_not_equal_m_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_ary_aget_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_code i = *pc;
  int regno = GETARG_A(i);
  int nargs = GETARG_C(i);
  mrb_value *regs = mrb->c->stack;
  const cpu_word_t aryno = regno;
  const cpu_word_t idxno = aryno + 1;
  mrbjit_reginfo *ainfo = &coi->reginfo[regno];
  mrbjit_reginfo *iinfo = &coi->reginfo[idxno];

  // No support 2 args or Index is not Fixnum
  if ((nargs > 1) ||
      (mrb_type(regs[idxno]) != MRB_TT_FIXNUM) ||
      mrb_ary_ptr(regs[aryno])->as.heap.aux.capa <= mrb_fixnum(regs[idxno])) {
    return mrb_nil_value();
  }

  inLocalLabel();
  gen_type_guard(mrb, idxno, status, pc, coi);
  gen_flush_regs(mrb, pc, status, coi, 1);
  //gen_flush_one(mrb, status, coi, aryno);

  ainfo->unboxedp = 0;
  ainfo->regplace = MRBJIT_REG_MEMORY;
  ainfo->type = MRB_TT_FREE;
  ainfo->klass = NULL;
  ainfo->constp = 0;

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
  
  if(iinfo->regplace == MRBJIT_REG_IMMIDATE &&
     iinfo->type == MRB_TT_FIXNUM) {
    mrb_int idx = mrb_fixnum(iinfo->value);

    
    gen_ary_ptr(mrb, status, coi);
    movsd(reg_dtmp0, ptr [reg_tmp1 + idx * sizeof(mrb_value)]);
    emit_local_var_write(mrb, coi, aryno, reg_dtmp0);
  }
  else {
    gen_ary_size(mrb, status, coi);
    emit_push(mrb, coi, reg_tmp0s);
    emit_local_var_value_read(mrb, coi, reg_tmp0s, idxno);
    test(reg_tmp0s, reg_tmp0s);
    jge(".normal");
    add(reg_tmp0s, dword [reg_sp]);
    jl(".retnil");
    L(".normal");
    emit_cmp(mrb, coi, reg_tmp0s, reg_sp, 0);
    jge(".retnil");

    emit_push(mrb, coi, reg_tmp0);
    gen_ary_ptr(mrb, status, coi);
    emit_pop(mrb, coi, reg_tmp0);
    test(reg_tmp1, reg_tmp1);
    jz(".retnil");

    movsd(reg_dtmp0, ptr [reg_tmp1 + reg_tmp0 * sizeof(mrb_value)]);
    emit_local_var_write(mrb, coi, aryno, reg_dtmp0);
    emit_jmp(mrb, coi, ".exit");

    L(".retnil");
    emit_load_literal(mrb, coi, reg_tmp0s, 0);
    emit_local_var_value_write(mrb, coi, aryno, reg_tmp0s);
    emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FALSE);
    emit_local_var_type_write(mrb, coi, aryno, reg_tmp0s);

    L(".exit");
    add(reg_sp, sizeof(cpu_word_t));
  }
  outLocalLabel();
  
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_ary_aget(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_ary_aget_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_ary_aset_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int regno = GETARG_A(i);
  int nargs = GETARG_C(i);
    mrb_value *regs = mrb->c->stack;
  const cpu_word_t aryno = regno;
  const cpu_word_t idxno = aryno + 1;
  const cpu_word_t valno = idxno + 1;

  if (nargs != 2 || 
      mrb_type(regs[idxno]) != MRB_TT_FIXNUM ||
      ARY_SHARED_P(mrb_ary_ptr(regs[aryno]))) {
    return mrb_nil_value();    	// Support only simple array set
  }

  inLocalLabel();
  gen_type_guard(mrb, idxno, status, pc, coi);
  gen_flush_regs(mrb, pc, status, coi, 1);
#if 1
  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
  gen_ary_size(mrb, status, coi);
  emit_push(mrb, coi, reg_tmp0s);
  emit_local_var_value_read(mrb, coi, reg_tmp0s, idxno);
  test(reg_tmp0s, reg_tmp0s);
  jge(".normal");
  add(reg_tmp0s, dword [reg_sp]);
  jl(".retnil");
  L(".normal");
  emit_cmp(mrb, coi, reg_tmp0s, reg_tmp1, OffsetOf(struct RArray, as.heap.len));
  jge(".retnil");

  emit_local_var_read(mrb, coi, reg_dtmp0, valno);

  emit_push(mrb, coi, reg_tmp0);
  gen_ary_ptr(mrb, status, coi);
  emit_pop(mrb, coi, reg_tmp0);

  movsd(ptr [reg_tmp1 + reg_tmp0 * sizeof(mrb_value)], reg_dtmp0);
  emit_local_var_write(mrb, coi, regno, reg_dtmp0);

  emit_local_var_type_read(mrb, coi, reg_tmp0s, valno);
  xor(reg_tmp0s, 0xfff00000);
  emit_cmp(mrb, coi, reg_tmp0s, MRB_TT_HAS_BASIC);
  jb(".exit");
  emit_cmp(mrb, coi, reg_tmp0s, MRB_TT_MAXDEFINE);
  ja(".exit");

  emit_cfunc_start(mrb, coi);

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 2, reg_tmp0);
  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, aryno);
  emit_arg_push(mrb, coi, 1, reg_tmp1);
  emit_arg_push(mrb, coi, 0, reg_mrb);
  emit_call(mrb, coi, (void *)mrb_field_write_barrier);
  emit_cfunc_end(mrb, coi, 3 * sizeof(void *));

  emit_jmp(mrb, coi, ".exit");

  L(".retnil");
#endif
  emit_cfunc_start(mrb, coi);

  emit_arg_push_nan(mrb, coi, 3, reg_tmp0, valno);

  emit_local_var_value_read(mrb, coi, reg_tmp0s, idxno);
  emit_arg_push(mrb, coi, 2, reg_tmp0);

  emit_arg_push_nan(mrb, coi, 1, reg_tmp0, aryno);

  emit_arg_push(mrb, coi, 0, reg_mrb);

  emit_call(mrb, coi, (void *)mrb_ary_set);
  emit_cfunc_end(mrb, coi, 2 * sizeof(void *) + 2 * sizeof(mrb_value));
  emit_local_var_write_from_cfunc(mrb, coi, regno);

  L(".exit");
  add(reg_sp, sizeof(cpu_word_t));

  outLocalLabel();
  
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_ary_aset(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_ary_aset_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_ary_first_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  mrb_code i = *pc;
  int regno = GETARG_A(i);
  int nargs = GETARG_C(i);

  // No support 1 args only no args.
  if (nargs > 1) {
    return mrb_nil_value();
  }

  /* Cant support zero array */
  if (!mrb_array_p(regs[regno]) ||
      RARRAY_LEN(regs[regno]) == 0) {
    return mrb_nil_value();
  }

  gen_flush_regs(mrb, pc, status, coi, 1);

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, regno);
  gen_ary_ptr(mrb, status, coi);

  emit_move(mrb, coi, reg_dtmp0, reg_tmp1, 0);
  emit_local_var_write(mrb, coi, regno, reg_dtmp0);
  
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_ary_first(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_ary_first_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_ary_size_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_code i = *pc;
  int regno = GETARG_A(i);
  int nargs = GETARG_C(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];


  // No support 1 args only no args.
  if (nargs > 0) {
    return mrb_nil_value();
  }

  //gen_flush_regs(mrb, pc, status, coi, 1);

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, regno);

  gen_ary_size(mrb, status, coi);

  emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);
  emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FIXNUM);
  emit_local_var_type_write(mrb, coi, regno, reg_tmp0s);
  
  dinfo->type = MRB_TT_FIXNUM;
  dinfo->klass = mrb->fixnum_class;
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_ary_size(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_ary_size_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

void
MRBJitCode::gen_call_initialize(mrb_state *mrb, mrb_value proc,
				mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);
  mrb_method_t m;
  mrb_value klass = regs[a];
  struct RClass *c = mrb_class_ptr(klass);
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  mrb_sym mid = mrb_intern_cstr(mrb, "initialize");

  m = mrb_method_search_vm(mrb, &c, mid);

  if (MRB_METHOD_CFUNC_P(m)) {
    CALL_CFUNC_BEGIN;
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)c);
    emit_arg_push(mrb, coi, 3, reg_tmp0);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)m);
    emit_arg_push(mrb, coi, 2, reg_tmp0);
    CALL_CFUNC_STATUS(mrbjit_exec_send_c_void, 2);
  }
  else {
    /* patch initialize method */
    mrb_irep *pirep = MRB_METHOD_PROC(m)->body.irep;
    mrb_code *piseq = pirep->iseq;
    int i;
    int j;


    if (pirep->jit_entry_tab && 0) {
      for (i = 0; i < pirep->ilen; i++) {
	for (j = 0; j < pirep->jit_entry_tab[i].size; j++) {
	  if (pirep->jit_entry_tab[i].body[j].reginfo) {
	    mrb_free(mrb, pirep->jit_entry_tab[i].body[j].reginfo);
	  }
	}
	mrb_free(mrb, pirep->jit_entry_tab->body);
	mrb_free(mrb, pirep->jit_entry_tab);
      }
    }

    pirep->jit_entry_tab = (mrbjit_codetab *)mrb_calloc(mrb, pirep->ilen, sizeof(mrbjit_codetab));
    for (i = 0; i < pirep->ilen; i++) {
      pirep->jit_entry_tab[i].size = 2;
      pirep->jit_entry_tab[i].body = 
	(mrbjit_code_info *)mrb_calloc(mrb, 2, sizeof(mrbjit_code_info));
    }

    for (int i = 0; i < pirep->ilen; i++) {
      if (GET_OPCODE(piseq[i]) == OP_RETURN && 
	  (piseq[i] & ((1 << 23) - 1)) != piseq[i]) {
	pirep->iseq = (mrb_code *)mrb_malloc(mrb, pirep->ilen *  sizeof(mrb_code));
	for (int j = 0; j < pirep->ilen; j++) {
	  pirep->iseq[j] = piseq[j];
	}
	if (!(pirep->flags & MRB_ISEQ_NO_FREE)) {
	  mrb_free(mrb, piseq);
	}
	piseq = pirep->iseq;
	break;
      }
    }

    for (int i = 0; i < pirep->ilen; i++) {
      if (GET_OPCODE(piseq[i]) == OP_RETURN) {
	/* clear A argument (return self always) */
	piseq[i] &= ((1 << 23) - 1);
      }
    }
    
    /* call info setup */
    if (GET_OPCODE(ins) == OP_TAILCALL) {
      int j;
      int n = GETARG_C(ins);
      if (n == 127) {
	n = 1;
      }

      /* move stack */
      for (j = 0; j < n + 2; j++) {
	emit_local_var_read(mrb, coi, reg_dtmp0, a + j);
	emit_local_var_write(mrb, coi, j, reg_dtmp0);
      }

      /* setup ci */
      emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));

      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), (cpu_word_t)mid);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, target_class), (cpu_word_t)c);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), 0);
      emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)m);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, proc), reg_tmp0);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp0);
      if (n == CALL_MAXARGS) {
	emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), -1);
      }
      else {
	emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), n);
      }

      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, nregs), (cpu_word_t)pirep->nregs);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), (cpu_word_t)pirep);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pool), (cpu_word_t)pirep->pool);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(syms), (cpu_word_t)pirep->syms);
      /* stack extend */
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)pirep->iseq);
    }
    else {
      gen_send_mruby(mrb, MRB_METHOD_PROC(m), mid, klass, c, status, pc, coi);
    }
    gen_exit(mrb, pirep->iseq, 1, 0, status, coi);
  }

  dinfo->type = MRB_TT_OBJECT;
  dinfo->klass = mrb_class_ptr(klass);
  dinfo->constp = 0;
}  


mrb_value
MRBJitCode::mrbjit_prim_instance_new_impl(mrb_state *mrb, mrb_value proc,
					  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  mrb_value klass = regs[a];
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  dinfo->unboxedp = 0;

  if (mrb_class_ptr(klass)->tt == MRB_TT_SCLASS) {
    return mrb_nil_value();
  }

  gen_flush_regs(mrb, pc, status, coi, 1);

  // TODO add guard of class
  
  // obj = mrbjit_instance_alloc(mrb, klass);
  emit_cfunc_start(mrb, coi);
  emit_arg_push_nan(mrb, coi, 1, reg_tmp0, a);
  emit_arg_push(mrb, coi, 0, reg_mrb);
  emit_call(mrb, coi, (void *)mrbjit_instance_alloc);
  emit_cfunc_end(mrb, coi, 3 * sizeof(void *));

  // regs[a] = obj;
  emit_local_var_write_from_cfunc(mrb, coi, a);

  gen_call_initialize(mrb, proc, status, coi);

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_instance_new(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_instance_new_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_mmm_instance_new_impl(mrb_state *mrb, mrb_value proc,
					  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  mrb_value klass = regs[a];
  struct RClass *c = mrb_class_ptr(klass);
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  dinfo->unboxedp = 0;

  gen_flush_regs(mrb, pc, status, coi, 1);

  // TODO add guard of class
  
  inLocalLabel();
  emit_push(mrb, coi, reg_vars);
  // obj = mrbjit_instance_alloc(mrb, klass);
  if (civoff >= 0) {
    emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
    emit_push(mrb, coi, reg_tmp0);			/* PUSH __objcache__ */
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, civoff * sizeof(mrb_value) + 4);
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, civoff * sizeof(mrb_value));
    test(reg_tmp0, reg_tmp0);
    jnz("@f");
    emit_pop(mrb, coi, reg_tmp0);	     /* POP __objcache__  dummy */
  }    

  emit_cfunc_start(mrb, coi);
  emit_arg_push_nan(mrb, coi, 1, reg_tmp0, a);
  emit_arg_push(mrb, coi, 0, reg_mrb);
  emit_call(mrb, coi, (void *)mrbjit_instance_alloc);
  emit_cfunc_end(mrb, coi, 3 * sizeof(void *));
  emit_local_var_write_from_cfunc(mrb, coi, a);
  emit_jmp(mrb, coi, ".allocend");

  L("@@");
  // regs[a] = obj;
  emit_local_var_ptr_value_read(mrb, coi, reg_vars, a);
  emit_local_var_write_from_cfunc(mrb, coi, a);

  if (civoff >= 0) {
    emit_pop(mrb, coi, reg_tmp0);			/* POP __objcache__ */ 
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, civoff * sizeof(mrb_value));
    emit_add(mrb, coi, reg_tmp1, reg_mrb);
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RObject, c));

    test(reg_tmp1, reg_tmp1);
    je(".freeobj_not_exist");

    emit_sub(mrb, coi, reg_tmp1, reg_mrb);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1);

    /*emit_load_literal(mrb, coi, reg_tmp1, 0xfff00000 | MRB_TT_OBJECT);
      emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value) + 4, reg_tmp1);*/

    emit_jmp(mrb, coi, ".update_end");
    L(".freeobj_not_exist");
    emit_load_literal(mrb, coi, reg_tmp1, 0);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1);
    emit_load_literal(mrb, coi, reg_tmp1, 0xfff00000 | MRB_TT_FALSE);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value) + 4, reg_tmp1);
    L(".update_end");

    emit_cfunc_start(mrb, coi);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)c);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_write_barrier);
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *));
  }

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, OffsetOf(struct RObject, c), reg_vars);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
  emit_load_literal(mrb, coi, reg_tmp1, 0);
  emit_move(mrb, coi, reg_tmp0, OffsetOf(struct iv_tbl, last_len), reg_tmp1);
    
  L(".allocend");
  emit_pop(mrb, coi, reg_vars);
  //  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  //emit_move(mrb, coi, reg_tmp0, OffsetOf(struct RArray, c), (cpu_word_t)c);

  gen_call_initialize(mrb, proc, status, coi);

  outLocalLabel();

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_mmm_instance_new(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_mmm_instance_new_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_mmm_move_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  struct RClass *c = mrb_obj_ptr(regs[a])->c;
  mrb_value klass = mrb_obj_value(c);
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, c));
  emit_cfunc_start(mrb, coi);
  emit_arg_push(mrb, coi, 1, reg_tmp0);
  emit_arg_push(mrb, coi, 0, reg_mrb);
  emit_call(mrb, coi, (void *)mrb_write_barrier);
  emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *));

  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, c));
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
  emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
  emit_local_var_read(mrb, coi, reg_dtmp0, a);
  emit_move(mrb, coi, reg_tmp1s, reg_tmp0, civoff * sizeof(mrb_value) + 4);

  and(reg_tmp1s, 0x7f);
  cmp(reg_tmp1s, MRB_TT_OBJECT);
  emit_load_literal(mrb, coi, reg_tmp1, 0);
  emit_jmp(mrb, coi, "@f");
  emit_move(mrb, coi, reg_tmp1, reg_tmp0, civoff * sizeof(mrb_value));
  emit_add(mrb, coi, reg_tmp1, reg_mrb);
  L("@@");
  emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_dtmp0);
  emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
  emit_move(mrb, coi, reg_tmp0, OffsetOf(struct RObject, c), reg_tmp1);

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_mmm_move(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_mmm_move_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fiber_resume_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;

  gen_flush_regs(mrb, pc, status, coi, 1);
  gen_exit(mrb, pc, 1, 1, status, coi);
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fiber_resume(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fiber_resume_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

extern "C" void disasm_irep(mrb_state *mrb, mrb_irep *irep);

mrb_value
MRBJitCode::mrbjit_prim_enum_all_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  mrb_sym *syms = *status->syms;
  int i = *pc;
  int a = GETARG_A(i);
  int n = GETARG_C(i);
  mrb_method_t m;
  struct RClass *c;
  mrb_value recv;
  mrb_sym mid = syms[GETARG_B(i)];
  int blk = (a + n + 1);
  mrb_irep *cirep;
  mrb_irep *nirep;
  mrbjit_reginfo *binfo = &coi->reginfo[blk];

  if (mrb_type(regs[blk]) != MRB_TT_PROC) {
    return mrb_nil_value();    	// without block
  }
  if (binfo->constp == 0) {
    return mrb_nil_value();    	// block is not literal
  }

  recv = regs[a];
  c = mrb_class(mrb, recv);
  m = mrb_method_search_vm(mrb, &c, mid);

  cirep = MRB_METHOD_PROC(m)->body.irep;
  nirep = mrb_add_irep(mrb);
  //  disasm_irep(mrb, birep);
  //disasm_irep(mrb, cirep);
  nirep->flags = cirep->flags;
  nirep->iseq = cirep->iseq;
  nirep->ilen = cirep->ilen;
  nirep->pool = cirep->pool;
  nirep->plen = cirep->plen;
  nirep->reps = cirep->reps;
  nirep->rlen = cirep->rlen;
  nirep->syms = cirep->syms;
  nirep->slen = cirep->slen;
  nirep->nregs = cirep->nregs;

  return mrb_obj_value(mrbjit_get_local_proc(mrb, nirep));
}

extern "C" mrb_value
mrbjit_prim_enum_all(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_enum_all_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_kernel_equal_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  mrb_value *regs = mrb->c->stack;
  int i = *pc;
  int a = GETARG_A(i);
  struct RClass *c = mrb_class(mrb, regs[a]);
  mrb_method_t m = mrb_method_search_vm(mrb, &c, mrb_intern_cstr(mrb, "=="));
  mrbjit_reginfo *dinfo = &coi->reginfo[a];

  dinfo->type = MRB_TT_TRUE;
  dinfo->klass = mrb->true_class;
  dinfo->constp = 0;
  dinfo->unboxedp = 0;


  return mrb_obj_value(MRB_METHOD_PROC(m));
}

extern "C" mrb_value
mrbjit_prim_kernel_equal(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_kernel_equal_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}


mrb_value
MRBJitCode::mrbjit_prim_kernel_block_given_p_impl(mrb_state *mrb, mrb_value proc,
			  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int a = GETARG_A(i);
  mrb_callinfo *ci = mrb->c->ci;
  mrbjit_reginfo *dinfo = &coi->reginfo[a];

  dinfo->type = MRB_TT_TRUE;
  dinfo->klass = mrb->true_class;
  dinfo->constp = 0;
  dinfo->unboxedp = 0;
  
  if (ci == mrb->c->cibase ||
      ci->proc->e.env) {
    return mrb_nil_value();    	// not in toplevel of method or the toplevel env
  }

  emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
  emit_move(mrb, coi, reg_tmp0s, reg_tmp1, OffsetOf(mrb_callinfo, argc));
  
  test(reg_tmp0s, reg_tmp0s);
  jg("@f");
  emit_load_literal(mrb, coi, reg_tmp0, 0);
  L("@@");

  inc(reg_tmp0);
  emit_add(mrb, coi, reg_tmp0, reg_tmp0);
  emit_add(mrb, coi, reg_tmp0, reg_tmp0);
  emit_add(mrb, coi, reg_tmp0, reg_tmp0);
  emit_add(mrb, coi, reg_tmp0, reg_regs);
  emit_move(mrb, coi, reg_dtmp0, reg_tmp0, 0);
  emit_local_var_write(mrb, coi, a, reg_dtmp0);

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_kernel_block_given_p(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_kernel_block_given_p_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_math_sqrt_impl(mrb_state *mrb, mrb_value proc,
					  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  int i = *pc;
  const int dst = GETARG_A(i);
  const int src = dst + 1;
  mrbjit_reginfo *dinfo = &coi->reginfo[dst];
  dinfo->unboxedp = 0;

  gen_type_guard(mrb, src, status, pc, coi);
  switch(mrb_type(regs[src])) {
  case MRB_TT_FLOAT:
    emit_local_var_read(mrb, coi, reg_dtmp0, src);
    break;

  case MRB_TT_FIXNUM:
    emit_local_var_int_value_read(mrb, coi, reg_dtmp0, src);
    break;

  default:
    return mrb_nil_value();
  }

  sqrtsd(reg_dtmp1, reg_dtmp0);
  emit_local_var_write(mrb, coi, dst, reg_dtmp1);
  dinfo->type = MRB_TT_FLOAT;
  dinfo->klass = mrb->float_class;

  return mrb_true_value();
}
  
extern "C" mrb_value
mrbjit_prim_math_sqrt(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_math_sqrt_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_math_callcfunc_impl(mrb_state *mrb, mrb_value proc,
					    mrbjit_vmstatus *status, mrbjit_code_info *coi, void *func)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  int i = *pc;
  const int dst = GETARG_A(i);
  const int src = dst + 1;
  mrbjit_reginfo *dinfo = &coi->reginfo[dst];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[src]) != MRB_TT_FLOAT) {
    return mrb_nil_value();
  }
  gen_type_guard(mrb, src, status, pc, coi);

  emit_cfunc_start(mrb, coi);
  emit_arg_push_nan(mrb, coi, 0, reg_tmp0, src);
  emit_call(mrb, coi, func);
  emit_cfunc_end(mrb, coi, sizeof(double));
  emit_local_var_write(mrb, coi, dst, reg_dtmp1);
  dinfo->type = MRB_TT_FLOAT;
  dinfo->klass = mrb->float_class;

  return mrb_true_value();
}
  
extern "C" mrb_value
mrbjit_prim_math_sin(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_math_callcfunc_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi, (void *)((double (*)(double))sin));
}


extern "C" mrb_value
mrbjit_prim_math_cos(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_math_callcfunc_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi, (void *)((double (*)(double))cos));
}

mrb_value
MRBJitCode::mrbjit_prim_numeric_minus_at_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code *pc = *status->pc;
  int i = *pc;
  const int dst = GETARG_A(i);
  const int src = dst;
  mrbjit_reginfo *dinfo = &coi->reginfo[dst];
  dinfo->unboxedp = 0;

  gen_flush_regs(mrb, pc, status, coi, 1);
  if (mrb_type(regs[src]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, src, status, pc, coi);
    emit_local_var_read(mrb, coi, reg_dtmp0, src);
    emit_sub(mrb, coi, reg_dtmp1, reg_dtmp1);
    emit_sub(mrb, coi, reg_dtmp1, reg_dtmp0);
    emit_local_var_write(mrb, coi, dst, reg_dtmp1);
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return mrb_true_value();
  }
  else if (mrb_type(regs[src]) == MRB_TT_FIXNUM) {
    gen_type_guard(mrb, src, status, pc, coi);
    emit_local_var_value_read(mrb, coi, reg_tmp0s, src);
    neg(reg_tmp0s);
    emit_local_var_value_write(mrb, coi, dst, reg_tmp0s);
    dinfo->type = MRB_TT_FIXNUM;
    dinfo->klass = mrb->fixnum_class;

    return mrb_true_value();
  }

  return mrb_nil_value();
}

extern "C" mrb_value
mrbjit_prim_numeric_minus_at(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_numeric_minus_at_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}


mrb_value
MRBJitCode::mrbjit_prim_str_plus_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  const int dstno = GETARG_A(i);
  const int srcno = dstno + 1;
  mrbjit_reginfo *dinfo = &coi->reginfo[dstno];
  dinfo->unboxedp = 0;

  gen_type_guard(mrb, srcno, status, pc, coi);

  emit_cfunc_start(mrb, coi);
  emit_arg_push_nan(mrb, coi, 2, reg_tmp0, srcno);
  emit_arg_push_nan(mrb, coi, 1, reg_tmp0, dstno);
  emit_arg_push(mrb, coi, 0, reg_mrb);
  emit_call(mrb, coi, (void *)mrb_str_plus);
  emit_cfunc_end(mrb, coi, sizeof(mrb_value)*2 + sizeof(mrb_state *));

  emit_local_var_write_from_cfunc(mrb, coi, dstno);

  dinfo->type = MRB_TT_STRING;
  dinfo->klass = mrb->string_class;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_str_plus(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_str_plus_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

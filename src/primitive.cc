#include "jitcode.h"
extern "C" {
#include "mruby.h"
#include "mruby/primitive.h"
#include "mruby/array.h"
#include "mruby/irep.h"
#include "mruby/variable.h"
#include "opcode.h"

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
  mrb_value *regs = *status->regs;
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

    emit_local_var_read(mrb, coi, xmm0, regno);
    emit_local_var_int_value_read(mrb, coi, xmm1, regno + 1);
    emit_cmp(mrb, coi, xmm0, xmm1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, regno, status, pc, coi);
    gen_type_guard(mrb, regno + 1, status, pc, coi);

    emit_local_var_int_value_read(mrb, coi, xmm0, regno);
    emit_local_var_read(mrb, coi, xmm1, regno + 1);
    emit_cmp(mrb, coi, xmm0, xmm1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, regno, status, pc, coi);
    gen_type_guard(mrb, regno + 1, status, pc, coi);

    emit_local_var_read(mrb, coi, xmm0, regno);
    emit_local_var_read(mrb, coi, xmm1, regno + 1);
    emit_cmp(mrb, coi, xmm0, xmm1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {
    emit_local_var_int_value_read(mrb, coi, xmm0, regno);
    emit_local_var_int_value_read(mrb, coi, xmm1, regno + 1);
    emit_cmp(mrb, coi, xmm0, xmm1);
    /*	  emit_local_var_value_read(mrb, coi, reg_tmp0, regno);
	  emit_cmp(mrb, coi, eax, ecx, off1);*/
  }
  else {
    return mrb_nil_value();
  }

  inLocalLabel();
  jnz(".cmpneq");

  emit_load_literal(mrb, coi, reg_tmp0, 0);
  emit_jmp(mrb, coi, ".cmpend");

  L(".cmpneq");
  jb(".cmplt");

  emit_load_literal(mrb, coi, reg_tmp0, 1);
  emit_jmp(mrb, coi, ".cmpend");

  L(".cmplt");
  emit_load_literal(mrb, coi, reg_tmp0, -1);

  L(".cmpend");
  outLocalLabel();

  emit_local_var_value_write(mrb, coi, regno, reg_tmp0);
  emit_load_literal(mrb, coi, reg_tmp0, 0xfff00000 | MRB_TT_FIXNUM);
  emit_local_var_type_write(mrb, coi, regno, reg_tmp0);
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
  mrb_value *regs = *status->regs;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM) {
    return mrb_nil_value();
  }
  gen_type_guard(mrb, regno, status, pc, coi);

  gen_type_guard(mrb, regno + 1, status, pc, coi);

  emit_local_var_value_read(mrb, coi, reg_tmp0, regno);
  emit_local_var_div(mrb, coi, regno + 1);
  test(eax, eax);
  setl(al);
  and(eax, 1);
  neg(eax);
  emit_load_literal(mrb, coi, reg_tmp0, 0);
  emit_sub(mrb, coi, edx, eax);
  emit_local_var_value_write(mrb, coi, regno, reg_tmp1);

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
MRBJitCode::mrbjit_prim_fix_to_f_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;
  int i = *pc;
  int regno = GETARG_A(i);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  emit_local_var_int_value_read(mrb, coi, xmm0, regno);
  emit_local_var_write(mrb, coi, regno, xmm0);
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

const void *
MRBJitCode::mrbjit_prim_obj_not_equal_aux(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code **ppc = status->pc;
  mrb_value *regs  = *status->regs;

  COMP_GEN(setnz(al), setnz(al));

  return NULL;
}

mrb_value
MRBJitCode::mrbjit_prim_obj_not_equal_m_impl(mrb_state *mrb, mrb_value proc,
					     mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code **ppc = status->pc;
  int regno = GETARG_A(**ppc);
  mrb_value *regs  = *status->regs;
  enum mrb_vtype tt = (enum mrb_vtype) mrb_type(regs[regno]);
  mrbjit_reginfo *dinfo = &coi->reginfo[regno];
  dinfo->unboxedp = 0;

  /* Import from class.h */
  switch (tt) {
  case MRB_TT_FIXNUM:
  case MRB_TT_FLOAT:
  case MRB_TT_STRING:
    if (mrbjit_prim_obj_not_equal_aux(mrb, proc, status, coi) == NULL) {
      dinfo->type = MRB_TT_TRUE;
      dinfo->klass = mrb->true_class;
      dinfo->constp = 0;
    }

    return mrb_true_value();

  default:
    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;

    return mrb_nil_value();
  }

  dinfo->type = MRB_TT_TRUE;
  dinfo->klass = mrb->true_class;
  dinfo->constp = 0;
  return mrb_nil_value();
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
  const Xbyak::uint32 aryno = regno;
  const Xbyak::uint32 idxno = aryno + 1;
  mrbjit_reginfo *ainfo = &coi->reginfo[regno];

  // No support 2 args or Index is not Fixnum
  if ((nargs > 1) ||
      (mrb_type((*status->regs)[regno + 1]) != MRB_TT_FIXNUM)) {
    return mrb_nil_value();
  }

  inLocalLabel();
  gen_class_guard(mrb, regno, status, pc, coi, NULL, 1);

  ainfo->unboxedp = 0;
  ainfo->regplace = MRBJIT_REG_MEMORY;
  ainfo->type = MRB_TT_FREE;
  ainfo->klass = NULL;
  ainfo->constp = 0;

  emit_local_var_value_read(mrb, coi, reg_tmp1, aryno);
  emit_local_var_value_read(mrb, coi, reg_tmp0, idxno);
  test(eax, eax);
  jge(".normal");
  add(eax, dword [edx + OffsetOf(struct RArray, len)]);
  jl(".retnil");
  L(".normal");
  emit_cmp(mrb, coi, eax, edx, OffsetOf(struct RArray, len));
  jge(".retnil");
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, ptr));
  test(edx, edx);
  jz(".retnil");
  movsd(xmm0, ptr [edx + eax * sizeof(mrb_value)]);
  emit_local_var_write(mrb, coi, aryno, xmm0);
  emit_jmp(mrb, coi, ".exit");

  L(".retnil");
  emit_load_literal(mrb, coi, reg_tmp0, 0);
  emit_local_var_value_write(mrb, coi, aryno, reg_tmp0);
  emit_load_literal(mrb, coi, reg_tmp0, 0xfff00000 | MRB_TT_FALSE);
  emit_local_var_type_write(mrb, coi, aryno, reg_tmp0);

  L(".exit");
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
  const Xbyak::uint32 aryno = regno;
  const Xbyak::uint32 idxno = aryno + 1;
  const Xbyak::uint32 valno = idxno + 1;

  if (nargs != 2) {
    return mrb_nil_value();    	// Support only 2 args(index, value)
  }

  inLocalLabel();
#if 0

  emit_local_var_value_read(mrb, coi, reg_tmp1, aryno);
  emit_local_var_value_read(mrb, coi, reg_tmp0, idxno);
  test(eax, eax);
  jge(".normal");
  add(eax, dword [edx + OffsetOf(struct RArray, len)]);
  jl(".retnil");
  L(".normal");
  emit_cmp(mrb, coi, eax, edx, OffsetOf(struct RArray, len));
  jge(".retnil");

  emit_local_var_read(mrb, coi, xmm0, valno);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, ptr));
  movsd(ptr [edx + eax * sizeof(mrb_value)], xmm0);

  emit_cfunc_start(mrb, coi);

  emit_local_var_type_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 3, reg_tmp0);
  emit_local_var_value_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 2, reg_tmp0);
  emit_local_var_value_read(mrb, coi, reg_tmp1, aryno);
  emit_arg_push(mrb, coi, 1, reg_tmp1);
  emit_arg_push(mrb, coi, 0, esi);
  call((void *)mrb_field_write_barrier);
  emit_cfunc_end(mrb, coi, 2 * sizeof(void *) + sizeof(mrb_value));

  emit_jmp(mrb, coi, ".exit");

  L(".retnil");
#endif
  emit_cfunc_start(mrb, coi);

  emit_local_var_type_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 5, eax);
  emit_local_var_value_read(mrb, coi, reg_tmp0, valno);
  emit_arg_push(mrb, coi, 4, eax);

  emit_local_var_value_read(mrb, coi, reg_tmp0, idxno);
  emit_arg_push(mrb, coi, 3, reg_tmp0);

  emit_local_var_type_read(mrb, coi, reg_tmp0, aryno);
  emit_arg_push(mrb, coi, 2, eax);
  emit_local_var_value_read(mrb, coi, reg_tmp0, aryno);
  emit_arg_push(mrb, coi, 1, eax);

  emit_arg_push(mrb, coi, 0, esi);

  call((void *)mrb_ary_set);
  emit_cfunc_end(mrb, coi, 2 * sizeof(void *) + 2 * sizeof(mrb_value));

  L(".exit");

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
  mrb_code i = *pc;
  int regno = GETARG_A(i);
  int nargs = GETARG_C(i);

  // No support 1 args only no args.
  if (nargs > 1) {
    return mrb_nil_value();
  }

  gen_class_guard(mrb, regno, status, pc, coi, NULL, 1);

  emit_local_var_value_read(mrb, coi, reg_tmp1, regno);
  emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RArray, ptr));
  emit_move(mrb, coi, xmm0, reg_tmp1, 0);
  emit_local_var_write(mrb, coi, regno, xmm0);
  
  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_ary_first(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_ary_first_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_instance_new_impl(mrb_state *mrb, mrb_value proc,
					  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  struct RProc *m;
  mrb_value klass = regs[a];
  struct RClass *c = mrb_class_ptr(klass);
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  mrb_sym mid = mrb_intern_cstr(mrb, "initialize");
  dinfo->unboxedp = 0;

  m = mrb_method_search_vm(mrb, &c, mid);

  // TODO add guard of class
  
  // obj = mrbjit_instance_alloc(mrb, klass);
  emit_cfunc_start(mrb, coi);
  emit_load_literal(mrb, coi, reg_tmp0, *((Xbyak::uint32 *)(&klass) + 1));
  emit_arg_push(mrb, coi, 2, eax);
  emit_load_literal(mrb, coi, reg_tmp0, *((Xbyak::uint32 *)(&klass)));
  emit_arg_push(mrb, coi, 1, eax);
  emit_arg_push(mrb, coi, 0, esi);
  call((void *)mrbjit_instance_alloc);
  emit_cfunc_end(mrb, coi, 3 * sizeof(void *));

  // regs[a] = obj;
  emit_local_var_value_write(mrb, coi, a, reg_tmp0);
  emit_local_var_type_write(mrb, coi, a, reg_tmp1);

  if (MRB_PROC_CFUNC_P(m)) {
    CALL_CFUNC_BEGIN;
    emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)c);
    emit_arg_push(mrb, coi, 3, eax);
    emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)m);
    emit_arg_push(mrb, coi, 2, eax);
    CALL_CFUNC_STATUS(mrbjit_exec_send_c, 2);
  }
  else {
    /* patch initialize method */
    mrb_irep *pirep = m->body.irep;
    mrb_code *piseq = pirep->iseq;
    for (unsigned int i = 0; i < pirep->ilen; i++) {
      if (GET_OPCODE(piseq[i]) == OP_RETURN && 
	  (piseq[i] & ((1 << 23) - 1)) != piseq[i]) {
	pirep->iseq = (mrb_code *)mrb_malloc(mrb, pirep->ilen *  sizeof(mrb_code));
	for (unsigned int j = 0; j < pirep->ilen; j++) {
	  pirep->iseq[j] = piseq[j];
	}
	if (!(pirep->flags & MRB_ISEQ_NO_FREE)) {
	  mrb_free(mrb, piseq);
	}
	piseq = pirep->iseq;
	break;
      }
    }

    for (unsigned int i = 0; i < pirep->ilen; i++) {
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
      emit_move(mrb, coi, reg_tmp1, edi, OffsetOf(mrb_context, ci));

      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), (Xbyak::uint32)mid);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, target_class), (Xbyak::uint32)c);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), 0);
      emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)m);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, proc), reg_tmp0);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp0);
      if (n == CALL_MAXARGS) {
	emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), -1);
      }
      else {
	emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), n);
      }

      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, nregs), (Xbyak::uint32)pirep->nregs);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), (Xbyak::uint32)pirep);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pool), (Xbyak::uint32)pirep->pool);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(syms), (Xbyak::uint32)pirep->syms);
      /* stack extend */
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (Xbyak::uint32)pirep->iseq);
    }
    else {
      gen_send_mruby(mrb, m, mid, klass, mrb_class_ptr(klass), status, pc, coi);
    }
    gen_exit(mrb, m->body.irep->iseq, 1, 0, status, coi);
  }

  dinfo->type = MRB_TT_OBJECT;
  dinfo->klass = mrb_class_ptr(klass);
  dinfo->constp = 0;

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
  mrb_value *regs = *status->regs;
  mrb_code *pc = *status->pc;
  mrb_code ins = *pc;
  int a = GETARG_A(ins);
  //int nargs = GETARG_C(ins);

  struct RProc *m;
  mrb_value klass = regs[a];
  struct RClass *c = mrb_class_ptr(klass);
  mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(ins)];
  mrb_sym mid = mrb_intern_cstr(mrb, "initialize");
  int civoff = mrbjit_iv_off(mrb, klass, mrb_intern_lit(mrb, "__objcache__"));

  dinfo->unboxedp = 0;
  m = mrb_method_search_vm(mrb, &c, mid);

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
  call((void *)mrbjit_instance_alloc);
  emit_cfunc_end(mrb, coi, 3 * sizeof(void *));

  L("@@");
  // regs[a] = obj;
  emit_local_var_value_write(mrb, coi, a, reg_tmp0);
  emit_local_var_type_write(mrb, coi, a, reg_tmp1);
  emit_move(mrb, coi, reg_tmp0, eax, OffsetOf(struct RObject, iv));
  emit_load_literal(mrb, coi, reg_tmp1, 0);
  emit_move(mrb, coi, reg_tmp0, OffsetOf(iv_tbl, last_len), reg_tmp1);

  if (civoff >= 0) {
    emit_pop(mrb, coi, eax);			/* POP __objcache__ */ 
    emit_load_literal(mrb, coi, reg_tmp1, 0);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value), reg_tmp1);
    emit_load_literal(mrb, coi, reg_tmp1, 0xfff00000 | MRB_TT_FALSE);
    emit_move(mrb, coi, reg_tmp0, civoff * sizeof(mrb_value) + 4, reg_tmp1);
  }    

  if (MRB_PROC_CFUNC_P(m)) {
    CALL_CFUNC_BEGIN;
    emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)c);
    emit_arg_push(mrb, coi, 1, eax);
    emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)m);
    emit_arg_push(mrb, coi, 0, eax);
    CALL_CFUNC_STATUS(mrbjit_exec_send_c, 2);
  }
  else {
    /* patch initialize method */
    mrb_irep *pirep = m->body.irep;
    mrb_code *piseq = pirep->iseq;
    for (unsigned int i = 0; i < pirep->ilen; i++) {
      if (GET_OPCODE(piseq[i]) == OP_RETURN && 
	  (piseq[i] & ((1 << 23) - 1)) != piseq[i]) {
	pirep->iseq = (mrb_code *)mrb_malloc(mrb, pirep->ilen *  sizeof(mrb_code));
	for (unsigned int j = 0; j < pirep->ilen; j++) {
	  pirep->iseq[j] = piseq[j];
	}
	if (!(pirep->flags & MRB_ISEQ_NO_FREE)) {
	  mrb_free(mrb, piseq);
	}
	piseq = pirep->iseq;
	break;
      }
    }

    for (unsigned int i = 0; i < pirep->ilen; i++) {
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
      emit_move(mrb, coi, reg_tmp1, edi, OffsetOf(mrb_context, ci));

      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), (Xbyak::uint32)mid);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, target_class), (Xbyak::uint32)c);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), 0);
      emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)m);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, proc), reg_tmp0);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp0);
      if (n == CALL_MAXARGS) {
	emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), -1);
      }
      else {
	emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), n);
      }

      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, nregs), (Xbyak::uint32)pirep->nregs);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), (Xbyak::uint32)pirep);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pool), (Xbyak::uint32)pirep->pool);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(syms), (Xbyak::uint32)pirep->syms);
      /* stack extend */
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (Xbyak::uint32)pirep->iseq);
    }
    else {
      gen_send_mruby(mrb, m, mid, klass, mrb_class_ptr(klass), status, pc, coi);
    }
    gen_exit(mrb, m->body.irep->iseq, 1, 0, status, coi);
  }

  dinfo->type = MRB_TT_OBJECT;
  dinfo->klass = mrb_class_ptr(klass);
  dinfo->constp = 0;

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_mmm_instance_new(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_mmm_instance_new_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fiber_resume_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;

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
  mrb_value *regs = *status->regs;
  mrb_sym *syms = *status->syms;
  int i = *pc;
  int a = GETARG_A(i);
  int n = GETARG_C(i);
  struct RProc *m;
  struct RClass *c;
  mrb_value recv;
  mrb_sym mid = syms[GETARG_B(i)];
  int blk = (a + n + 1);
  mrb_irep *cirep;
  mrb_irep *nirep;
  mrb_irep *birep;
  mrbjit_reginfo *binfo = &coi->reginfo[blk];

  if (mrb_type(regs[blk]) != MRB_TT_PROC) {
    return mrb_nil_value();    	// without block
  }
  if (binfo->constp == 0) {
    return mrb_nil_value();    	// block is not literal
  }
  birep = mrb_proc_ptr(regs[blk])->body.irep;

  recv = regs[a];
  c = mrb_class(mrb, recv);
  m = mrb_method_search_vm(mrb, &c, mid);

  cirep = m->body.irep;
  nirep = mrb_add_irep(mrb);
  disasm_irep(mrb, birep);
  disasm_irep(mrb, cirep);
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
  mrb_value *regs = *status->regs;
  int i = *pc;
  int a = GETARG_A(i);
  struct RClass *c = mrb_class(mrb, regs[a]);
  struct RProc *m = mrb_method_search_vm(mrb, &c, mrb_intern_cstr(mrb, "=="));
  mrbjit_reginfo *dinfo = &coi->reginfo[a];

  dinfo->type = MRB_TT_TRUE;
  dinfo->klass = mrb->true_class;
  dinfo->constp = 0;
  dinfo->unboxedp = 0;


  return mrb_obj_value(m);
}

extern "C" mrb_value
mrbjit_prim_kernel_equal(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_kernel_equal_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_math_sqrt_impl(mrb_state *mrb, mrb_value proc,
					  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
  mrb_code *pc = *status->pc;
  int i = *pc;
  const int dst = GETARG_A(i);
  const int src = dst + 1;
  mrbjit_reginfo *dinfo = &coi->reginfo[dst];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[src]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, src, status, pc, coi);
    emit_local_var_read(mrb, coi, xmm0, src);
    sqrtsd(xmm0, xmm0);
    emit_local_var_write(mrb, coi, dst, xmm0);
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return mrb_true_value();
  }
  else if (mrb_type(regs[src]) == MRB_TT_FIXNUM) {
    gen_type_guard(mrb, src, status, pc, coi);
    emit_local_var_int_value_read(mrb, coi, xmm0, src);
    sqrtsd(xmm0, xmm0);
    emit_local_var_write(mrb, coi, dst, xmm0);
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return mrb_true_value();
  }

  return mrb_nil_value();
}
  
extern "C" mrb_value
mrbjit_prim_math_sqrt(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_math_sqrt_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_numeric_minus_at_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
  mrb_code *pc = *status->pc;
  int i = *pc;
  const int dst = GETARG_A(i);
  const int src = dst;
  mrbjit_reginfo *dinfo = &coi->reginfo[dst];
  dinfo->unboxedp = 0;

  if (mrb_type(regs[src]) == MRB_TT_FLOAT) {
    gen_type_guard(mrb, src, status, pc, coi);
    emit_local_var_read(mrb, coi, xmm0, src);
    emit_sub(mrb, coi, xmm1, xmm1);
    emit_sub(mrb, coi, xmm1, xmm0);
    emit_local_var_write(mrb, coi, dst, xmm1);
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return mrb_true_value();
  }
  else if (mrb_type(regs[src]) == MRB_TT_FIXNUM) {
    gen_type_guard(mrb, src, status, pc, coi);
    emit_local_var_value_read(mrb, coi, eax, src);
    neg(eax);
    emit_local_var_value_write(mrb, coi, dst, eax);
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

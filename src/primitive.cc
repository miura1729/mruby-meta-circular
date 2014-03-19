#include "jitcode.h"
extern "C" {
#include "mruby.h"
#include "mruby/primitive.h"
#include "mruby/array.h"
#include "mruby/irep.h"
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
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);
  const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);
  // not need guard for self because guard geneate already
  //mov(eax, dword [ecx + off0 + 4]);
  //gen_type_guard(mrb, (enum mrb_vtype)mrb_type(regs[regno]), pc);

  gen_type_guard(mrb, regno + 1, status, pc, coi);
  
  if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&
      mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {
    movsd(xmm0, ptr [ecx + off0]);
    cvtsi2sd(xmm1, ptr [ecx + off1]);
    comisd(xmm0, xmm1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {
    cvtsi2sd(xmm0, ptr [ecx + off0]);
    movsd(xmm1, ptr [ecx + off1]);
    comisd(xmm0, xmm1);
  }
  else if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&
	   mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {
    movsd(xmm0, ptr [ecx + off0]);
    movsd(xmm1, ptr [ecx + off1]);
    comisd(xmm0, xmm1);
  }
  else {
    cvtsi2sd(xmm0, ptr [ecx + off0]);
    cvtsi2sd(xmm1, ptr [ecx + off1]);
    comisd(xmm0, xmm1);
    /*    mov(eax, dword [ecx + off0]);
	  cmp(eax, dword [ecx + off1]);*/
  }

  inLocalLabel();
  jnz(".cmpneq");

  xor(eax, eax);
  jmp(".cmpend");

  L(".cmpneq");
  jb(".cmplt");

  xor(eax, eax);
  inc(eax);
  jmp(".cmpend");

  L(".cmplt");
  xor(eax, eax);
  dec(eax);

  L(".cmpend");
  outLocalLabel();

  mov(dword [ecx + off0], eax);
  mov(dword [ecx + off0 + 4], 0xfff80000 | MRB_TT_FIXNUM);

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
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);

  add(dword [ecx + off0], 1);

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
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);
  const Xbyak::uint32 off1 = (regno + 1) * sizeof(mrb_value);

  if (mrb_type(regs[regno]) != MRB_TT_FIXNUM ||
      mrb_type(regs[regno + 1]) != MRB_TT_FIXNUM) {
    return mrb_nil_value();
  }
  gen_type_guard(mrb, regno, status, pc, coi);

  gen_type_guard(mrb, regno + 1, status, pc, coi);

  mov(eax, ptr [ecx + off0]);
  mov(edx, eax);
  sar(edx, (sizeof(void *) * 8) - 1);
  idiv(ptr [ecx + off1]);
  test(eax, eax);
  setl(al);
  and(eax, 1);
  neg(eax);
  xor(edx, eax);
  sub(edx, eax);
  mov(ptr [ecx + off0], edx);

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
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);

  mov(eax, dword [ecx + off0]);
  cvtsi2sd(xmm0, eax);
  movsd(ptr [ecx + off0], xmm0);

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_fix_to_f(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_fix_to_f_impl(mrb, proc, (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_obj_not_equal_m_impl(mrb_state *mrb, mrb_value proc,
					     mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code **ppc = status->pc;
  mrb_value *regs = *status->regs;

  COMP_GEN(setnz, setnz);

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
  const Xbyak::uint32 offary = regno * sizeof(mrb_value);
  const Xbyak::uint32 offidx = offary + sizeof(mrb_value);

  // No support 2 args or Index is not Fixnum
  if ((nargs > 1) ||
      (mrb_type((*status->regs)[regno + 1]) != MRB_TT_FIXNUM)) {
    return mrb_nil_value();
  }

  inLocalLabel();
  gen_class_guard(mrb, regno, status, pc, coi);

  mov(edx, ptr [ecx + offary]);
  mov(eax, ptr [ecx + offidx]);
  test(eax, eax);
  jge(".normal");
  add(eax, dword [edx + OffsetOf(struct RArray, len)]);
  jl(".retnil");
  L(".normal");
  cmp(eax, dword [edx + OffsetOf(struct RArray, len)]);
  jge(".retnil");
  mov(edx, dword [edx + OffsetOf(struct RArray, ptr)]);
  test(edx, edx);
  jz(".retnil");
  movsd(xmm0, ptr [edx + eax * sizeof(mrb_value)]);
  movsd(ptr [ecx + offary], xmm0);
  jmp(".exit");

  L(".retnil");
  xor(eax, eax);
  mov(dword [ecx + offary], eax);
  mov(dword [ecx + offary + 4], 0xfff00000 | MRB_TT_FALSE);

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
  const Xbyak::uint32 offary = regno * sizeof(mrb_value);
  const Xbyak::uint32 offidx = offary + sizeof(mrb_value);
  const Xbyak::uint32 offval = offidx + sizeof(mrb_value);

  if (nargs != 2) {
    return mrb_nil_value();    	// Support only 2 args(index, value)
  }

  inLocalLabel();

  push(ecx);

  mov(eax, ptr [ecx + offval + 4]);
  push(eax);
  mov(eax, ptr [ecx + offval]);
  push(eax);

  mov(edx, ptr [ecx + offary]);
  mov(eax, ptr [ecx + offidx]);
  test(eax, eax);
  jge(".normal");
  add(eax, dword [edx + OffsetOf(struct RArray, len)]);
  jl(".retnil");
  L(".normal");
  cmp(eax, dword [edx + OffsetOf(struct RArray, len)]);
  jg(".retnil");
  push(eax);

  mov(eax, ptr [ecx + offary + 4]);
  push(eax);
  mov(eax, ptr [ecx + offary]);
  push(eax);

  push(esi);

  call((void *)mrb_ary_set);
  add(esp, 2 * sizeof(void *) + 2 * sizeof(mrb_value));
  pop(ecx);
  movsd(xmm0, ptr [ecx + offval]);
  movsd(ptr [ecx + offary], xmm0);
  jmp(".exit");

  L(".retnil");
  add(esp, sizeof(void *) + sizeof(mrb_value)); // ecx, val
  xor(eax, eax);
  mov(dword [ecx + offary], eax);
  mov(dword [ecx + offary + 4], 0xfff00000 | MRB_TT_FALSE);

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
MRBJitCode::mrbjit_prim_instance_new_impl(mrb_state *mrb, mrb_value proc,
					  mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
  mrb_irep *irep = *status->irep;
  mrb_code *pc = *status->pc;
  int i = *pc;
  int a = GETARG_A(i);
  //int nargs = GETARG_C(i);

  struct RProc *m;
  mrb_value klass = regs[a];
  struct RClass *c = mrb_class_ptr(klass);

  m = mrb_method_search_vm(mrb, &c, mrb_intern_cstr(mrb, "initialize"));

  // TODO add guard of class
  
  // obj = mrbjit_instance_alloc(mrb, klass);
  push(ecx);
  push(ebx);
  mov(eax, *((Xbyak::uint32 *)(&klass) + 1));
  push(eax);
  mov(eax, *((Xbyak::uint32 *)(&klass)));
  push(eax);
  push(esi);
  call((void *)mrbjit_instance_alloc);
  add(esp, 3 * sizeof(void *));
  pop(ebx);
  pop(ecx);

  // regs[a] = obj;
  mov(ptr [ecx + a * sizeof(mrb_value)], eax);
  mov(ptr [ecx + a * sizeof(mrb_value) + 4], edx);

  if (MRB_PROC_CFUNC_P(m)) {
    CALL_CFUNC_BEGIN;
    mov(eax, (Xbyak::uint32)c);
    push(eax);
    mov(eax, (Xbyak::uint32)m);
    push(eax);
    CALL_CFUNC_STATUS(mrbjit_exec_send_c, 2);
  }
  else {
    /* patch initialize method */
    mrb_irep *pirep = m->body.irep;
    mrb_code *piseq = pirep->iseq;
    for (unsigned int i = 0; i < pirep->ilen; i++) {
      if (GET_OPCODE(piseq[i]) == OP_RETURN) {
	/* clear A argument (return self always) */
	piseq[i] &= ((1 << 23) - 1);
      }
    }
    
    /* call info setup */
    CALL_CFUNC_BEGIN;
    mov(eax, (Xbyak::uint32)c);
    push(eax);
    mov(eax, (Xbyak::uint32)m);
    push(eax);
    CALL_CFUNC_STATUS(mrbjit_exec_send_mruby, 2);

    mov(ecx, dword [ebx + VMSOffsetOf(regs)]);

    gen_set_jit_entry(mrb, pc, coi, irep);

    gen_exit(m->body.irep->iseq, 1, 0, status);
  }

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_instance_new(mrb_state *mrb, mrb_value proc, void *status, void *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_instance_new_impl(mrb, proc,  (mrbjit_vmstatus *)status, (mrbjit_code_info *)coi);
}

mrb_value
MRBJitCode::mrbjit_prim_fiber_resume_impl(mrb_state *mrb, mrb_value proc,
				      mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  mrb_code *pc = *status->pc;

  gen_exit(pc, 1, 1, status);
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

#include "jitcode.h"
extern "C" {
#include "mruby/primitive.h"
}

mrb_value
MRBJitCode::mrbjit_prim_num_cmp_impl(mrb_state *mrb, mrb_value proc) {
  mrbjit_vmstatus *status = mrb->vmstatus;
  mrb_code *pc = *status->pc;
  mrb_value *regs = *status->regs;
  int i = *pc;
  int regno = GETARG_A(i);
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);
  const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);
  mov(eax, dword [ecx + off0 + 4]); /* Get type tag */
  gen_type_guard(mrb, (enum mrb_vtype)mrb_type(regs[regno]), pc);
  mov(eax, dword [ecx + off1 + 4]); /* Get type tag */
  gen_type_guard(mrb, (enum mrb_vtype)mrb_type(regs[regno + 1]), pc);
  
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

  mov(eax, 1);
  jmp(".cmpend");

  L(".cmplt");
  mov(eax, -1);

  L(".cmpend");
  outLocalLabel();

  mov(dword [ecx + off0], eax);
  mov(eax, 0xfff00000 | MRB_TT_FIXNUM);
  mov(dword [ecx + off0 + 4], eax);

  return mrb_true_value();
}

extern "C" mrb_value
mrbjit_prim_num_cmp(mrb_state *mrb, mrb_value proc)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;

  return code->mrbjit_prim_num_cmp_impl(mrb, proc);
}


#ifndef MRUBY_JIT_X86_H
#define MRUBY_JIT_X86_H

#include <xbyak/xbyak.h>
extern "C" {
#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/value.h"
}

/* Regs Map                               *
 * ecx   -- pointer to regs               *
 * ebx   -- pointer to status->pc         *
 * esi   -- pointer to mrb                *
 * edi   -- pointer to mrb->c             */

class MRBGenericCodeGenerator: public Xbyak::CodeGenerator {
 public:
  Xbyak::Reg32 reg_regs;	/* ecx */
  Xbyak::Reg32 reg_vars;	/* ebx */
  Xbyak::Reg32 reg_mrb;		/* esi */
  Xbyak::Reg32 reg_context;	/* edo */

  Xbyak::Reg32 reg_tmp0;	/* eax */
  Xbyak::Reg32 reg_tmp1;	/* edx */
  Xbyak::Xmm reg_dtmp0;		/* xmm0 */
  Xbyak::Xmm reg_dtmp1;		/* xmm1 */
  

  MRBGenericCodeGenerator() 
    :CodeGenerator(1024 * 1024)
  {
    reg_regs = ecx;
    reg_vars = ebx;
    reg_mrb = esi;
    reg_context = edi;

    reg_tmp0 = eax;
    reg_tmp1 = edx;
    
    reg_dtmp0 = xmm0;
    reg_dtmp1 = xmm1;
  }
  
  void
    gen_flush_literal(mrb_state *mrb, mrbjit_code_info *coi, const Xbyak::uint32 dstno) 
  {
    mrbjit_reginfo *dinfo = &coi->reginfo[dstno];

    emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)dinfo->value.value.p);
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0);
    if (mrb_type(dinfo->value) == MRB_TT_FLOAT ||
	dinfo->type != mrb_type(dinfo->value)) {
      emit_load_literal(mrb, coi, reg_tmp0, (Xbyak::uint32)dinfo->value.value.ttt);
      emit_local_var_type_write(mrb, coi, dstno, reg_tmp0);
      dinfo->type = mrb_type(dinfo->value);
    }
    dinfo->constp = 1;
    dinfo->unboxedp = 0;
  }

#define DEREF_REGNO(regno)                                           \
  ({								     \
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];                    \
    if (rinfo->regplace == MRBJIT_REG_IMMIDATE) {		     \
      gen_flush_literal(mrb, coi, regno);			     \
      rinfo->regplace = MRBJIT_REG_MEMORY;	                     \
    }                                                                \
    if (rinfo->regplace == MRBJIT_REG_AL) {                          \
      emit_bool_boxing(mrb, coi, reg_tmp0);                          \
      emit_local_var_type_write(mrb, coi, regno, reg_tmp0);          \
      rinfo->regplace = MRBJIT_REG_MEMORY;                           \
    }                                                                \
    (rinfo->regplace > MRBJIT_REG_VMREG0) ? rinfo->regplace - MRBJIT_REG_VMREG0 : regno; \
  })


  void emit_local_var_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, int regno)
  {
    regno = DEREF_REGNO(regno);
    movsd(dst, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_local_var_int_value_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, int regno)
  {
    regno = DEREF_REGNO(regno);
    cvtsi2sd(dst, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_local_var_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Xmm src)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    rinfo->regplace = MRBJIT_REG_MEMORY;
    movsd(ptr [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_local_var_value_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, int regno)
  {
    regno = DEREF_REGNO(regno);
    mov(dst, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_local_var_value_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg32 src)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    rinfo->regplace = MRBJIT_REG_MEMORY;
    mov(ptr [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_local_var_type_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, int regno)
  {
    regno = DEREF_REGNO(regno);
    mov(dst, ptr [reg_regs + regno * sizeof(mrb_value) + 4]);
  }

  void emit_local_var_type_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg32 src)
  {
    //mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    //rinfo->regplace = MRBJIT_REG_MEMORY;
    mov(ptr [reg_regs + regno * sizeof(mrb_value) + 4], src);
  }

  void emit_vm_var_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, int regno)
  {
    mov(dst, ptr [reg_vars + regno]);
  }

  void emit_vm_var_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg32 src)
  {
    mov(ptr [reg_vars + regno], src);
  }

  void emit_vm_var_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::uint32 src)
  {
    mov(dword [reg_vars + regno], src);
  }

  void emit_load_literal(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::uint32 src) {
    switch(src) {
    case -2:
      xor(dst, dst);
      dec(dst);
      dec(dst);
      break;

    case -1:
      xor(dst, dst);
      dec(dst);
      break;

    case 0:
      xor(dst, dst);
      break;

    case 1:
      xor(dst, dst);
      inc(dst);
      break;

    case 2:
      xor(dst, dst);
      inc(dst);
      inc(dst);
      break;

    default:
      mov(dst, src);
      break;
    }
  }

  void emit_cfunc_start(mrb_state *mrb, mrbjit_code_info *coi)
  {
    push(ecx);
    push(ebx);
  }

  void emit_cfunc_end(mrb_state *mrb, mrbjit_code_info *coi, int unwindsize)
  {
    add(esp, unwindsize);
    pop(ebx);
    pop(ecx);
  }

  void emit_arg_push(mrb_state *mrb, mrbjit_code_info *coi, int no, Xbyak::Reg32 reg)
  {
    push(reg);
  }
  void emit_arg_push(mrb_state *mrb, mrbjit_code_info *coi, int no, Xbyak::uint32 val)
  {
    push(val);
  }

  void emit_jmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 reg)
  {
    jmp(reg);
  }

  void emit_jmp(mrb_state *mrb, mrbjit_code_info *coi, const void *addr)
  {
    jmp(addr);
  }

  void emit_jmp(mrb_state *mrb, mrbjit_code_info *coi, const char *label)
  {
    jmp(label);
  }

  void emit_push(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 reg)
  {
    push(reg);
  }

  void emit_pop(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 reg)
  {
    pop(reg);
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      mov(dst, ptr [base]);
    }
    else {
      mov(dst, ptr [base + offset]);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::Reg32 src) {
    if (offset == 0) {
      mov(ptr [base], src);
    }
    else {
      mov(ptr [base + offset], src);
    }
  }

  void emit_movew(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::Reg16 src) {
    mov(word [base + offset], src);
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::uint32 src) {
    if (offset == 0) {
      mov(dword [base], src);
    }
    else {
      mov(dword [base + offset], src);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      movsd(dst, ptr [base]);
    }
    else {
      movsd(dst, ptr [base + offset]);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::Xmm src) {
    if (offset == 0) {
      movsd(ptr [base], src);
    }
    else {
      movsd(ptr [base + offset], src);
    }
  }

  void emit_bool_boxing(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst) {
    xor(ah, ah);
    cwde();
    add(eax, eax);
    or(eax, 0xfff00001);
    if (dst != eax) {
      mov(dst, eax);
    }
  }

  void emit_local_var_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 src, int regno) {
    regno = DEREF_REGNO(regno);
    cmp(src, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_local_var_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm src, int regno) {
    regno = DEREF_REGNO(regno);
    comisd(src, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 src, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      cmp(src, ptr [base]);
    }
    else {
      cmp(src, ptr [base + offset]);
    }
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 srcr, Xbyak::uint32 src) {
    cmp(srcr, src);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 src0, Xbyak::Reg32 src1) {
    cmp(src0, src1);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm src0, Xbyak::Xmm src1) {
    cmp(src0, src1);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm src, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      comisd(src, ptr [base]);
    }
    else {
      comisd(src, ptr [base + offset]);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::uint32 src) {
    add(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 src) {
    add(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Xmm src) {
    addsd(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      add(dst, ptr [base]);
    }
    else {
      add(dst, ptr [base + offset]);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::Reg32 src) {
    if (offset == 0) {
      add(ptr [base], src);
    }
    else {
      add(ptr [base + offset], src);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::uint32 src) {
    if (offset == 0) {
      add(dword [base], src);
    }
    else {
      add(dword [base + offset], src);
    }
  }

  void emit_local_var_add(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::uint32 src) {
    regno = DEREF_REGNO(regno);
    add(dword [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::uint32 src) {
    sub(dst, src);
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 src) {
    sub(dst, src);
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Xmm src) {
    subsd(dst, src);
  }

  void emit_mul(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 src) {
    imul(dst, src);
  }

  void emit_mul(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Xmm src) {
    mulsd(dst, src);
  }

  void emit_div(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Xmm src) {
    divsd(dst, src);
  }

  /* eax / (VM regs regno) -> edx, eax */
  void emit_local_var_div(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::uint32 regno) {
    regno = DEREF_REGNO(regno);
    cdq();
    idiv(ptr [ecx + regno * sizeof(mrb_value)]);
  }
};


#endif

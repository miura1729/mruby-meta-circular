#ifndef MRUBY_JIT_X86_H
#define MRUBY_JIT_X86_H

#include <xbyak/xbyak.h>
extern "C" {
#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/value.h"
}

typedef Xbyak::uint64 cpu_word_t;

/* Regs Map                               *
 * r12   -- pointer to regs               *
 * rbx   -- pointer to status->pc         *
 * r13   -- pointer to mrb                *
 * r14   -- pointer to mrb->c             *
 * r15   -- address of mrbjit_instance_alloc */

class MRBGenericCodeGenerator: public Xbyak::CodeGenerator {
  Xbyak::Reg64 argpos2reg[6];
  Xbyak::Reg64 reg_itmp;	/* rdi */
  Xbyak::Reg32 reg_itmps;	/* edi */

 public:
  Xbyak::Reg64 reg_regs;	/* r12 */
  Xbyak::Reg64 reg_vars;	/* rbx */
  Xbyak::Reg64 reg_mrb;		/* r13 */
  Xbyak::Reg64 reg_context;	/* r14 */
  Xbyak::Reg64 reg_sp;	        /* rsp */

  Xbyak::Reg64 reg_tmp0;	/* rax */
  Xbyak::Reg64 reg_tmp1;	/* rdx */
  Xbyak::Reg32 reg_tmp0s;	/* eax (32bit version)*/
  Xbyak::Reg32 reg_tmp1s;	/* edx (32bit version)*/
  Xbyak::Xmm reg_dtmp0;		/* xmm0 */
  Xbyak::Xmm reg_dtmp1;		/* xmm1 */
  Xbyak::Xmm xmmtab[8];

  MRBGenericCodeGenerator() 
    :CodeGenerator(1024 * 1024 * 40)
  {
    reg_regs = r12;
    reg_vars = rbx;
    reg_mrb = r13;
    reg_context = r14;
    reg_sp = rsp;

    reg_tmp0 = rax;
    reg_tmp0s = eax;
    reg_tmp1 = rdx;
    reg_tmp1s = edx;
    
    reg_dtmp0 = xmm0;
    reg_dtmp1 = xmm1;

    reg_itmp = rdi;
    reg_itmps = edi;
    argpos2reg[0] = rdi;
    argpos2reg[1] = rsi;
    argpos2reg[2] = rdx;
    argpos2reg[3] = rcx;
    argpos2reg[4] = r8;
    argpos2reg[5] = r9;
    xmmtab[0] = xmm0;
    xmmtab[1] = xmm1;
    xmmtab[2] = xmm2;
    xmmtab[3] = xmm3;
    xmmtab[4] = xmm4;
    xmmtab[5] = xmm5;
    xmmtab[6] = xmm6;
    xmmtab[7] = xmm7;
  }
  
  void
    gen_flush_literal(mrb_state *mrb, mrbjit_code_info *coi, const cpu_word_t dstno) 
  {
    mrbjit_reginfo *dinfo = &coi->reginfo[dstno];

    emit_load_literal(mrb, coi, reg_itmp, *reinterpret_cast<cpu_word_t *>(&dinfo->value.f));
    emit_local_var_write(mrb, coi, dstno, reg_itmp);
    dinfo->constp = 1;
    dinfo->unboxedp = 0;
  }

  void
    gen_flush_xmm(mrb_state *mrb, mrbjit_code_info *coi, const cpu_word_t dstno) 
  {
    mrbjit_reginfo *dinfo = &coi->reginfo[dstno];

    movsd(ptr [reg_regs + dstno * sizeof(mrb_value)],
	  xmmtab[dinfo->regplace - MRBJIT_REG_XMM0]);
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
    if (MRBJIT_REG_XMM1 <= rinfo->regplace &&                        \
        rinfo->regplace <= MRBJIT_REG_XMM7) {                        \
      movsd(ptr [reg_regs + regno * sizeof(mrb_value)],              \
	    xmmtab[rinfo->regplace - MRBJIT_REG_XMM0]);		     \
    }                                                                \
    if (rinfo->regplace == MRBJIT_REG_AL) {                          \
      emit_bool_boxing(mrb, coi, reg_itmps);                         \
      emit_local_var_type_write(mrb, coi, regno, reg_itmps);         \
      rinfo->regplace = MRBJIT_REG_MEMORY;                           \
    }                                                                \
    (rinfo->regplace >= MRBJIT_REG_VMREG0) ? rinfo->regplace - MRBJIT_REG_VMREG0 : regno; \
  })

  void emit_local_var_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, int regno)
  {
    regno = DEREF_REGNO(regno);
    movsd(dst, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_local_var_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, int regno)
  {
    regno = DEREF_REGNO(regno);
    mov(dst, ptr [reg_regs + regno * sizeof(mrb_value)]);
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

  void emit_local_var_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg64 src)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    rinfo->regplace = MRBJIT_REG_MEMORY;
    mov(ptr [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_local_var_write_from_cfunc(mrb_state *mrb, mrbjit_code_info *coi, int regno)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    rinfo->regplace = MRBJIT_REG_MEMORY;
    emit_local_var_write(mrb, coi, regno, reg_tmp0);
  }

  void emit_local_var_value_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, int regno)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    if (rinfo->regplace == MRBJIT_REG_IMMIDATE) {
      mov(dst, (cpu_word_t)rinfo->value.value.p);
    }
    else {
      regno = DEREF_REGNO(regno);
      mov(dst, ptr [reg_regs + regno * sizeof(mrb_value)]);
    }
  }

  void emit_local_var_ptr_value_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, int regno)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    if (rinfo->regplace == MRBJIT_REG_IMMIDATE) {
      lea(dst, ptr [reg_mrb + (cpu_word_t)rinfo->value.value.p]);
    }
    else {
      regno = DEREF_REGNO(regno);
      movsxd(dst, dword [reg_regs + regno * sizeof(mrb_value)]);
      add(dst, reg_mrb);
    }
  }

  void emit_local_var_value_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg32 src)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    rinfo->regplace = MRBJIT_REG_MEMORY;
    mov(ptr [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_local_var_value_write_noopt(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg32 src)
  {
    mov(ptr [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_local_var_type_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, int regno)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    if (rinfo->regplace == MRBJIT_REG_IMMIDATE) {
      mov(dst, (cpu_word_t)rinfo->value.value.ttt);
    }
    else {
      regno = DEREF_REGNO(regno);
      mov(dst, ptr [reg_regs + regno * sizeof(mrb_value) + 4]);
    }
  }

  void emit_local_var_type_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg32 src)
  {
    //mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    //rinfo->regplace = MRBJIT_REG_MEMORY;
    mov(ptr [reg_regs + regno * sizeof(mrb_value) + 4], src);
  }

  void emit_vm_var_read(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, int regno)
  {
    mov(dst, ptr [reg_vars + regno]);
  }

  void emit_vm_var_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, Xbyak::Reg64 src)
  {
    mov(ptr [reg_vars + regno], src);
  }

  void emit_vm_var_write(mrb_state *mrb, mrbjit_code_info *coi, int regno, cpu_word_t src)
  {
    mov(reg_itmp, src);
    mov(qword [reg_vars + regno], reg_itmp);
  }

  void emit_load_literal(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, cpu_word_t src) {
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

  void emit_load_literal(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, uint32_t src) {
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

  void emit_load_label(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, const char *src) {
    mov(dst, src);
  }

  void emit_load_literal_noopt(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, cpu_word_t src) {
    const int idx = dst.getIdx();
    int code = 176 | 1 << 3;
    db(dst.getRex());
    db(code | (idx & 7));
    db(src, 8);
  }

  void emit_cfunc_start(mrb_state *mrb, mrbjit_code_info *coi)
  {
    push(r12);
    push(rbx);
  }

  void emit_cfunc_end(mrb_state *mrb, mrbjit_code_info *coi, int unwindsize)
  {
    //    add(rsp, unwindsize);
    pop(rbx);
    pop(r12);
  }

  void emit_arg_push(mrb_state *mrb, mrbjit_code_info *coi, int no, Xbyak::Reg64 reg)
  {
    mov(argpos2reg[no], reg);
  }


  void emit_arg_push(mrb_state *mrb, mrbjit_code_info *coi, int no, cpu_word_t val)
  {
    mov(argpos2reg[no], val);
  }

  void emit_arg_push_from_cfunc(mrb_state *mrb, mrbjit_code_info *coi, int no)
  {
    mov(argpos2reg[no], rax);
  }

  void emit_arg_push_nan(mrb_state *mrb, mrbjit_code_info *coi, int no, Xbyak::Reg64 tmpreg, cpu_word_t regno)
  {
    emit_local_var_read(mrb, coi, tmpreg, regno);
    mov(argpos2reg[no], tmpreg);
  }

  void emit_call(mrb_state *mrb, mrbjit_code_info *coi, void *addr)
  {
    intptr_t off;
    off = ((uintptr_t)addr) - ((uintptr_t)mrbjit_instance_alloc);
    lea(rax, ptr [r15 + off]);
    call(rax);
  }

  void emit_jmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 reg)
  {
    jmp(reg);
  }

  void emit_jmp(mrb_state *mrb, mrbjit_code_info *coi, const void *addr)
  {
    jmp(addr, T_NEAR);
  }

  void emit_jmp(mrb_state *mrb, mrbjit_code_info *coi, const char *label)
  {
    jmp(label, T_NEAR);
  }

  void emit_push(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 reg)
  {
    push(reg);
  }

  void emit_pop(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 reg)
  {
    pop(reg);
  }


  void emit_push(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 reg)
  {
    push(reg);
  }

  void emit_pop(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 reg)
  {
    pop(reg);
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      mov(dst, ptr [base]);
    }
    else {
      mov(dst, ptr [base + offset]);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      mov(dst, ptr [base]);
    }
    else {
      mov(dst, ptr [base + offset]);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, Xbyak::Reg64 src) {
    if (offset == 0) {
      mov(ptr [base], src);
    }
    else {
      mov(ptr [base + offset], src);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, Xbyak::Reg32 src) {
    if (offset == 0) {
      mov(ptr [base], src);
    }
    else {
      mov(ptr [base + offset], src);
    }
  }

  void emit_movew(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, Xbyak::Reg16 src) {
    mov(word [base + offset], src);
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, cpu_word_t src) {
    if (base != reg_tmp0) {
      mov(reg_tmp0, src);
      mov(qword [base + offset], reg_tmp0);
    }
    else {
      mov(reg_itmp, src);
      mov(qword [base + offset], reg_itmp);
    }
  }

  void emit_moves(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, uint32_t src) {
    if (offset == 0) {
      mov(dword [base], src);
    }
    else {
      mov(dword [base + offset], src);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      movsd(dst, ptr [base]);
    }
    else {
      movsd(dst, ptr [base + offset]);
    }
  }

  void emit_move(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, Xbyak::Xmm src) {
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
    mrbjit_reginfo *rinfo = &coi->reginfo[regno];
    if (rinfo->regplace == MRBJIT_REG_IMMIDATE) {
      if (rinfo->value.value.p == 0) {
	test(src, src);
      }
      else {
	cmp(src, (cpu_word_t)rinfo->value.value.p);
      }
    }
    else {
      regno = DEREF_REGNO(regno);
      cmp(src, ptr [reg_regs + regno * sizeof(mrb_value)]);
    }
  }

  void emit_local_var_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm src, int regno) {
    regno = DEREF_REGNO(regno);
    comisd(src, ptr [reg_regs + regno * sizeof(mrb_value)]);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 src, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      cmp(src, ptr [base]);
    }
    else {
      cmp(src, ptr [base + offset]);
    }
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 src, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      cmp(src, ptr [base]);
    }
    else {
      cmp(src, ptr [base + offset]);
    }
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 srcr, uint32_t src) {
    cmp(srcr, src);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 srcr, uint64_t src) {
    mov(r8, src);
    cmp(srcr, r8);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 src0, Xbyak::Reg64 src1) {
    cmp(src0, src1);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 src0, Xbyak::Reg32 src1) {
    cmp(src0, src1);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm src0, Xbyak::Xmm src1) {
    comisd(src0, src1);
  }

  void emit_cmp(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm src, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      comisd(src, ptr [base]);
    }
    else {
      comisd(src, ptr [base + offset]);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, cpu_word_t src) {
      add(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, cpu_word_t src) {
    if (src == 1) {
      inc(dst);
    }
    else {
      add(dst, src);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 src) {
    add(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, Xbyak::Reg64 src) {
    add(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Xmm dst, Xbyak::Xmm src) {
    addsd(dst, src);
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg64 base, cpu_word_t offset) {
    if (offset == 0) {
      add(dst, ptr [base]);
    }
    else {
      add(dst, ptr [base + offset]);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, Xbyak::Reg32 src) {
    if (offset == 0) {
      add(ptr [base], src);
    }
    else {
      add(ptr [base + offset], src);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, Xbyak::Reg64 src) {
    if (offset == 0) {
      add(ptr [base], src);
    }
    else {
      add(ptr [base + offset], src);
    }
  }

  void emit_add(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 base, cpu_word_t offset, uint32_t src) {
    if (src == 1) {
      inc(dword [base + offset]);
    }
    else {
      if (offset == 0) {
	add(dword [base], src);
      }
      else {
	add(dword [base + offset], src);
      }
    }
  }

  void emit_local_var_add(mrb_state *mrb, mrbjit_code_info *coi, int regno, cpu_word_t src) {
    regno = DEREF_REGNO(regno);
    add(dword [reg_regs + regno * sizeof(mrb_value)], src);
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, cpu_word_t src) {
    sub(dst, src);
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, cpu_word_t src) {
    if (src == 1) {
      dec(dst);
    }
    else {
      sub(dst, src);
    }
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg32 dst, Xbyak::Reg32 src) {
    sub(dst, src);
  }

  void emit_sub(mrb_state *mrb, mrbjit_code_info *coi, Xbyak::Reg64 dst, Xbyak::Reg64 src) {
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
  void emit_local_var_div(mrb_state *mrb, mrbjit_code_info *coi, cpu_word_t regno) {
    regno = DEREF_REGNO(regno);
    cdq();
    idiv(ptr [reg_regs + regno * sizeof(mrb_value)]);
  }
};


#endif

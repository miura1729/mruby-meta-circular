#ifndef MRUBY_JIT_X86_H
#define MRUBY_JIT_X86_H

#include <xbyak/xbyak.h>

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
    :CodeGenerator(1024 * 1024 * 10)
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

  void emit_local_var_read(Xbyak::Xmm dst, int regno)
  {
    movsd(dst, ptr [reg_regs + regno]);
  }

  void emit_local_var_write(int regno, Xbyak::Xmm src)
  {
    movsd(ptr [reg_regs + regno], src);
  }

  void emit_local_var_value_read(Xbyak::Reg32 dst, int regno)
  {
    mov(dst, ptr [reg_regs + regno]);
  }

  void emit_local_var_value_write(int regno, Xbyak::Reg32 src)
  {
    mov(ptr [reg_regs + regno], src);
  }

  void emit_local_var_type_read(Xbyak::Reg32 dst, int regno)
  {
    mov(dst, ptr [reg_regs + regno + 4]);
  }

  void emit_local_var_type_write(int regno, Xbyak::Reg32 src)
  {
    mov(ptr [reg_regs + regno + 4], src);
  }

  void emit_vm_var_read(Xbyak::Reg32 dst, int regno)
  {
    mov(dst, ptr [reg_vars + regno]);
  }

  void emit_vm_var_write(int regno, Xbyak::Reg32 src)
  {
    mov(ptr [reg_vars + regno], src);
  }

  void emit_vm_var_write(int regno, Xbyak::uint32 src)
  {
    mov(dword [reg_vars + regno], src);
  }

  void emit_load_literal(Xbyak::Reg32 dst, Xbyak::uint32 src) {
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

  void emit_cfunc_start()
  {
    push(ecx);
    push(ebx);
  }

  void emit_cfunc_end(int unwindsize)
  {
    add(esp, unwindsize);
    pop(ebx);
    pop(ecx);
  }

  void emit_jmp(const void *addr)
  {
    jmp(addr);
  }

  void emit_jmp(const char *label)
  {
    jmp(label);
  }

  void emit_push(Xbyak::Reg32 reg)
  {
    push(reg);
  }

  void emit_pop(Xbyak::Reg32 reg)
  {
    pop(reg);
  }

  void emit_move(Xbyak::Reg32 dst, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      mov(dst, ptr [base]);
    }
    else {
      mov(dst, ptr [base + offset]);
    }
  }

  void emit_move(Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::Reg32 src) {
    if (offset == 0) {
      mov(ptr [base], src);
    }
    else {
      mov(ptr [base + offset], src);
    }
  }

  void emit_move(Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::uint32 src) {
    if (offset == 0) {
      mov(dword [base], src);
    }
    else {
      mov(dword [base + offset], src);
    }
  }

  void emit_move(Xbyak::Xmm dst, Xbyak::Reg32 base, Xbyak::uint32 offset) {
    if (offset == 0) {
      movsd(dst, ptr [base]);
    }
    else {
      movsd(dst, ptr [base + offset]);
    }
  }

  void emit_move(Xbyak::Reg32 base, Xbyak::uint32 offset, Xbyak::Xmm src) {
    if (offset == 0) {
      movsd(ptr [base], src);
    }
    else {
      movsd(ptr [base + offset], src);
    }
  }

};


#endif

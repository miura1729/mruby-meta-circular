/*
** mruby/jitcode.h - Class for XBYAK
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JITCOD_H
#define MRUBY_JITCODE_H

#include <xbyak/xbyak.h>
extern "C" {
#include "mruby.h"
#include "opcode.h"

#include "mruby/irep.h"
#include "mruby/value.h"
#include "mruby/jit.h"
} /* extern "C" */

/* Regs Map                               *
 * ecx   -- pointer to regs               *
 * ebx   -- pointer to pc                 */
class MRBJitCode: public Xbyak::CodeGenerator {

 public:

 MRBJitCode():
  CodeGenerator(1024)
  {
  }

  const void *
    gen_entry(mrb_state *mrb, mrb_irep *irep) 
  {
    const void* func_ptr = getCurr();
    return func_ptr;
  }

  void 
    gen_exit(mrb_code *pc) 
  {
    mov(dword [ebx], (Xbyak::uint32)pc);
    ret();
  }
  
  void 
    gen_jump_block(void *entry) 
  {
    jmp(entry);
  }

  void gen_type_guard(enum mrb_vtype tt, mrb_code *pc)
  {
    /* Input eax for type tag */
    if (tt == MRB_TT_FLOAT) {
      cmp(eax, 0xfff00000);
      ja("@f");
    } 
    else {
      cmp(eax, 0xfff00000 | tt);
      jz("@f");
    }

    /* Guard fail exit code */
    mov(dword [ebx], (Xbyak::uint32)pc);
    ret();

    L("@@");
  }

  const void *
    emit_move(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    movsd(xmm0, ptr [ecx + srcoff]);
    movsd(ptr [ecx + dstoff], xmm0);
    return code;
  }

  const void *
    emit_loadl(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 srcoff = GETARG_Bx(**ppc) * sizeof(mrb_value);
    mov(eax, (Xbyak::uint32)irep->pool + srcoff);
    movsd(xmm0, ptr [eax]);
    movsd(ptr [ecx + dstoff], xmm0);

    return code;
  }

  const void *
    emit_loadi(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) 
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 src = GETARG_sBx(**ppc);
    mov(eax, src);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_FIXNUM);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_loadt(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) 
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mov(eax, 1);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_TRUE);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_loadf(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) 
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mov(eax, 1);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_FALSE);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_loadnil(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) 
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    xor(eax, eax);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_FALSE);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_addi(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    const Xbyak::uint32 y = GETARG_C(**ppc);
    const Xbyak::uint32 off = GETARG_A(**ppc) * sizeof(mrb_value);
    int regno = GETARG_A(**ppc);
    mov(eax, dword [ecx + off + 4]); /* Get type tag */
    gen_type_guard((enum mrb_vtype)mrb_type(regs[regno]), *ppc);

    mov(eax, dword [ecx + off]);
    add(eax, y);
    mov(dword [ecx + off], eax);

    return code;
  }
};

#endif  /* MRUBY_JITCODE_H */

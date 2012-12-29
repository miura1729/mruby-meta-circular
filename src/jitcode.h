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
  CodeGenerator(1024 * 1024)
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

  void 
    gen_type_guard(enum mrb_vtype tt, mrb_code *pc)
  {
    /* Input eax for type tag */
    if (tt == MRB_TT_FLOAT) {
      cmp(eax, 0xfff00000);
      jb("@f");
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

  void
    gen_bool_guard(int b, mrb_code *pc)
  {
    /* Input eax for tested boolean */
    cmp(eax, 0xfff00001);
    if (b) {
      jnz("@f");
    } 
    else {
      jz("@f");
    }

    /* Guard fail exit code */
    mov(dword [ebx], (Xbyak::uint32)pc);
    ret();

    L("@@");
  }

  const void *
    emit_nop(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    return code;
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

    jno("@f");
    sub(esp, 8);
    movsd(qword [esp], xmm1);
    mov(eax, dword [ecx + off]);
    cvtsi2sd(xmm0, eax);
    mov(eax, y);
    cvtsi2sd(xmm1, eax);
    addsd(xmm0, xmm1);
    movsd(dword [ecx + off], xmm0);
    movsd(xmm1, ptr [esp]);
    add(esp, 8);
    L("@@");

    return code;
  }

#define COMP_GEN_II(CMPINST)                                         \
do {                                                                 \
    mov(eax, dword [ecx + off0]);                                    \
    cmp(eax, dword [ecx + off1]);                                    \
    CMPINST(al);						     \
    cwde();                                                          \
    add(eax, eax);                                                   \
    add(eax, 0xfff00001);                                            \
    mov(dword [ecx + off0 + 4], eax);                                \
    mov(dword [ecx + off0], 1);                                      \
} while(0)

#define COMP_GEN_IF(CMPINST)                                         \
do {                                                                 \
    cvtsi2sd(xmm0, ptr [ecx + off0]);                                \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST(al);						     \
    cwde();                                                          \
    add(eax, eax);                                                   \
    add(eax, 0xfff00001);                                            \
    mov(dword [ecx + off0 + 4], eax);                                \
    mov(dword [ecx + off0], 1);                                      \
} while(0)

#define COMP_GEN_FI(CMPINST)                                         \
do {                                                                 \
    sub(esp, 8); 					             \
    movsd(qword [esp], xmm1);   				     \
    movsd(xmm0, ptr [ecx + off0]);				     \
    cvtsi2sd(xmm1, ptr [ecx + off1]);                                \
    comisd(xmm0, xmm1);     			                     \
    CMPINST(al);						     \
    cwde();                                                          \
    add(eax, eax);                                                   \
    add(eax, 0xfff00001);                                            \
    mov(dword [ecx + off0 + 4], eax);                                \
    mov(dword [ecx + off0], 1);                                      \
    movsd(xmm1, ptr [esp]);					     \
    add(esp, 8);						     \
} while(0)

#define COMP_GEN_FF(CMPINST)                                         \
do {                                                                 \
    movsd(xmm0, dword [ecx + off0]);                                 \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST(al);						     \
    cwde();                                                          \
    add(eax, eax);                                                   \
    add(eax, 0xfff00001);                                            \
    mov(dword [ecx + off0 + 4], eax);                                \
    mov(dword [ecx + off0], 1);                                      \
} while(0)
    
#define COMP_GEN(CMPINSTI, CMPINSTF)				     \
do {                                                                 \
    int regno = GETARG_A(**ppc);                                     \
    const Xbyak::uint32 off0 = regno * sizeof(mrb_value);            \
    const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);             \
    mov(eax, dword [ecx + off0 + 4]); /* Get type tag */             \
    gen_type_guard((enum mrb_vtype)mrb_type(regs[regno]), *ppc);     \
    mov(eax, dword [ecx + off1 + 4]); /* Get type tag */             \
    gen_type_guard((enum mrb_vtype)mrb_type(regs[regno + 1]), *ppc); \
                                                                     \
    if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&                     \
             mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {           \
          COMP_GEN_FI(CMPINSTF);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&               \
             mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {            \
          COMP_GEN_IF(CMPINSTF);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&                \
             mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {            \
          COMP_GEN_FF(CMPINSTF);                                     \
    }                                                                \
    else {                                                           \
          COMP_GEN_II(CMPINSTI);                                     \
    }                                                                \
 } while(0)
  
  const void *
    emit_eq(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    COMP_GEN(setz, setz);

    return code;
  }

  const void *
    emit_lt(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    COMP_GEN(setl, setb);

    return code;
  }

  const void *
    emit_le(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    COMP_GEN(setle, setbe);

    return code;
  }

  const void *
    emit_gt(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    COMP_GEN(setg, seta);

    return code;
  }

  const void *
    emit_ge(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    COMP_GEN(setge, setae);

    return code;
  }

  const void *
    emit_jmpif(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
  {
    const void *code = getCurr();
    const int cond = GETARG_A(**ppc);
    const Xbyak::uint32 coff =  cond * sizeof(mrb_value);
    
    mov(eax, ptr [ecx + coff + 4]);
    if (mrb_test(regs[cond])) {
      gen_bool_guard(1, *ppc + 1);
    }
    else {
      gen_bool_guard(0, *ppc + GETARG_sBx(**ppc));
    }

    return code;
  }

  const void *
    emit_jmpnot(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
  {
    const void *code = getCurr();
    const int cond = GETARG_A(**ppc);
    const Xbyak::uint32 coff =  cond * sizeof(mrb_value);
    
    mov(eax, ptr [ecx + coff + 4]);
    if (!mrb_test(regs[cond])) {
      gen_bool_guard(0, *ppc + 1);
    }
    else {
      gen_bool_guard(1, *ppc + GETARG_sBx(**ppc));
    }

    return code;
  }
};

#endif  /* MRUBY_JITCODE_H */

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
#include "mruby/variable.h"
#include "mruby/proc.h"
#include "mruby/class.h"
#include "mruby/jit.h"

void mrbjit_exec_send(mrb_state *, mrbjit_vmstatus *);
void mrbjit_exec_enter(mrb_state *, mrbjit_vmstatus *);
void mrbjit_exec_return(mrb_state *, mrbjit_vmstatus *);
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
    xor(eax, eax);
    ret();
  }
  
  void 
    gen_jump_block(void *entry) 
  {
    jmp(entry);
  }

  void 
    gen_jmp(mrb_state *mrb, mrb_irep *irep, mrb_code *curpc, mrb_code *newpc) 
  {
    mrbjit_code_info *ci;
    int n = ISEQ_OFFSET_OF(newpc);
    if (irep->ilen < NO_INLINE_METHOD_LEN) {
      ci = search_codeinfo_prev(irep->jit_entry_tab + n, curpc, mrb->ci->pc);
    }
    else {
      ci = search_codeinfo_prev(irep->jit_entry_tab + n, curpc, NULL);
      mrb->compile_info.nest_level = 0;
    }
    if (ci) {
      if (ci->entry) {
	jmp((void *)ci->entry);
      }
      else {
	gen_exit(newpc);
      }
      mrb->compile_info.code_base = NULL;
    }
  }

  void 
    gen_type_guard(mrb_state *mrb, enum mrb_vtype tt, mrb_code *pc)
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
    xor(eax, eax);
    ret();

    L("@@");
  }

  void
    gen_bool_guard(mrb_state *mrb, int b, mrb_code *pc)
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
    xor(eax, eax);
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
    emit_loadself(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) 
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);

    movsd(xmm0, ptr [ecx]);
    movsd(ptr [ecx + dstoff], xmm0);
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
    emit_getiv(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const int idpos = GETARG_Bx(**ppc);
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 2 * sizeof(void *);

    push(ecx);
    push(ebx);
    push((Xbyak::uint32)irep->syms[idpos]);
    push((Xbyak::uint32)mrb);
    call((void *)mrb_vm_iv_get);
    add(sp, argsize);
    pop(ebx);
    pop(ecx);
    mov(dword [ecx + dstoff], eax);
    mov(dword [ecx + dstoff + 4], edx);

    return code;
  }

  const void *
    emit_setiv(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const int idpos = GETARG_Bx(**ppc);
    const Xbyak::uint32 srcoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 4 * sizeof(void *);

    push(ecx);
    push(ebx);
    mov(eax, dword [ecx + srcoff + 4]);
    push(eax);
    mov(eax, dword [ecx + srcoff]);
    push(eax);
    push((Xbyak::uint32)irep->syms[idpos]);
    push((Xbyak::uint32)mrb);
    call((void *)mrb_vm_iv_set);
    add(sp, argsize);
    pop(ebx);
    pop(ecx);

    return code;
  }

  const void *
    emit_getconst(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int sympos = GETARG_Bx(**ppc);
    const mrb_value v = mrb_vm_const_get(mrb, irep->syms[sympos]);

    mov(dword [ecx + dstoff], v.value.i);
    mov(dword [ecx + dstoff + 4], v.ttt);
    
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

#define OffsetOf(s_type, field) ((size_t) &((s_type *)0)->field) 
#define CALL_MAXARGS 127

#define CALL_CFUNC_STATUS(func_name)                                 \
  do {                                                               \
    push(ecx);                                                       \
    push(ebx);                                                       \
    mov(eax, ptr[esp + 12]);                                         \
    push(eax);                                                       \
\
    /* Update pc */                                                  \
    mov(eax, dword [eax + OffsetOf(mrbjit_vmstatus, pc)]);           \
    mov(dword [eax], (Xbyak::uint32)(*status->pc));                  \
\
    push((Xbyak::uint32)mrb);                                        \
    call((void *)func_name);                                         \
    add(esp, 8);                                                     \
    pop(ebx);                                                        \
    pop(ecx);                                                        \
\
    cmp(eax, eax);                                                   \
    jz("@f");                                                        \
    mov(dword [ebx], (Xbyak::uint32)(*status->pc));                  \
    ret();                                                           \
    L("@@");                                                         \
  }while (0)

  const void *
    emit_send(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();
    CALL_CFUNC_STATUS(mrbjit_exec_send);

    mov(eax, ptr[esp + 4]);
    mov(eax, dword [eax + OffsetOf(mrbjit_vmstatus, regs)]);
    mov(ecx, dword [eax]);

    return code;
  }

  const void *
    emit_enter(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();
    CALL_CFUNC_STATUS(mrbjit_exec_enter);

    mrb->compile_info.nest_level++;
    return code;
  }

  const void *
    emit_return(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();
    mrb->compile_info.nest_level--;
    if (mrb->compile_info.nest_level < 0) {
      return NULL;
    }

    CALL_CFUNC_STATUS(mrbjit_exec_return);

    mov(eax, ptr[esp + 4]);
    mov(eax, dword [eax + OffsetOf(mrbjit_vmstatus, regs)]);
    mov(ecx, dword [eax]);

    return code;
  }

#define OVERFLOW_CHECK_GEN(AINSTF)                                      \
    jno("@f");                                                          \
    sub(esp, 8);                                                        \
    movsd(qword [esp], xmm1);                                           \
    mov(eax, dword [ecx + reg0off]);                                    \
    cvtsi2sd(xmm0, eax);                                                \
    mov(eax, dword [ecx + reg1off]);                                    \
    cvtsi2sd(xmm1, eax);                                                \
    AINSTF(xmm0, xmm1);                                                 \
    movsd(dword [ecx + reg0off], xmm0);                                 \
    movsd(xmm1, ptr [esp]);                                             \
    add(esp, 8);                                                        \
    L("@@");                                                            \


#define ARTH_GEN(AINSTI, AINSTF)                                        \
  do {                                                                  \
    int reg0pos = GETARG_A(**ppc);                                      \
    int reg1pos = reg0pos + 1;                                          \
    const Xbyak::uint32 reg0off = reg0pos * sizeof(mrb_value);          \
    const Xbyak::uint32 reg1off = reg1pos * sizeof(mrb_value);          \
    enum mrb_vtype r0type = (enum mrb_vtype) mrb_type(regs[reg0pos]);   \
    enum mrb_vtype r1type = (enum mrb_vtype) mrb_type(regs[reg1pos]);   \
\
    if (r0type != r1type) {                                             \
      return NULL;                                                      \
    }                                                                   \
    mov(eax, dword [ecx + reg0off + 4]); /* Get type tag */             \
    gen_type_guard(mrb, r0type, *ppc);					\
    mov(eax, dword [ecx + reg1off + 4]); /* Get type tag */             \
    gen_type_guard(mrb, r1type, *ppc);					\
\
    if (r0type == MRB_TT_FIXNUM && r1type == MRB_TT_FIXNUM) {           \
      mov(eax, dword [ecx + reg0off]);                                  \
      AINSTI(eax, dword [ecx + reg1off]);			        \
      mov(dword [ecx + reg0off], eax);                                  \
      OVERFLOW_CHECK_GEN(AINSTF);                                       \
    }                                                                   \
    else if (r0type == MRB_TT_FLOAT && r1type == MRB_TT_FLOAT) {	\
      movsd(xmm0, ptr [ecx + reg0off]);                                 \
      AINSTF(xmm0, ptr [ecx + reg1off]);				\
      movsd(ptr [ecx + reg0off], xmm0);                                 \
    }                                                                   \
    else {                                                              \
      mov(dword [ebx], (Xbyak::uint32)*ppc);                            \
      xor(eax, eax);                                                    \
      ret();                                                            \
    }                                                                   \
} while(0)

  const void *
    emit_add(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    ARTH_GEN(add, addsd);
    return code;
  }

  const void *
    emit_sub(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    ARTH_GEN(sub, subsd);
    return code;
  }

  const void *
    emit_mul(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    ARTH_GEN(imul, mulsd);
    return code;
  }

  const void *
    emit_div(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    return code;
  }

#define OVERFLOW_CHECK_I_GEN(AINSTF)                                    \
    jno("@f");                                                          \
    sub(esp, 8);                                                        \
    movsd(qword [esp], xmm1);                                           \
    mov(eax, dword [ecx + off]);                                        \
    cvtsi2sd(xmm0, eax);                                                \
    mov(eax, y);                                                        \
    cvtsi2sd(xmm1, eax);                                                \
    AINSTF(xmm0, xmm1);                                                 \
    movsd(dword [ecx + off], xmm0);                                     \
    movsd(xmm1, ptr [esp]);                                             \
    add(esp, 8);                                                        \
    L("@@");                                                            \

#define ARTH_I_GEN(AINSTI, AINSTF)                                      \
  do {                                                                  \
    const Xbyak::uint32 y = GETARG_C(**ppc);                            \
    const Xbyak::uint32 off = GETARG_A(**ppc) * sizeof(mrb_value);      \
    int regno = GETARG_A(**ppc);                                        \
    enum mrb_vtype atype = (enum mrb_vtype) mrb_type(regs[regno]);      \
    mov(eax, dword [ecx + off + 4]); /* Get type tag */                 \
    gen_type_guard(mrb, atype, *ppc);					\
\
    if (atype == MRB_TT_FIXNUM) {                                       \
      mov(eax, dword [ecx + off]);                                      \
      AINSTI(eax, y);                                                   \
      mov(dword [ecx + off], eax);                                      \
      OVERFLOW_CHECK_I_GEN(AINSTF);                                     \
    }                                                                   \
    else if (atype == MRB_TT_FLOAT) {					\
      sub(esp, 8);                                                      \
      movsd(qword [esp], xmm1);                                         \
      movsd(xmm0, ptr [ecx + off]);                                     \
      mov(eax, y);                                                      \
      cvtsi2sd(xmm1, eax);                                              \
      AINSTF(xmm0, xmm1);                                               \
      movsd(ptr [ecx + off], xmm0);                                     \
      movsd(xmm1, ptr [esp]);                                           \
      add(esp, 8);                                                      \
    }                                                                   \
    else {                                                              \
      mov(dword [ebx], (Xbyak::uint32)*ppc);                            \
      xor(eax, eax);                                                    \
      ret();                                                            \
    }                                                                   \
} while(0)
    
  const void *
    emit_addi(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    ARTH_I_GEN(add, addsd);
    return code;
  }

  const void *
    emit_subi(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs) 
  {
    const void *code = getCurr();
    ARTH_I_GEN(sub, subsd);
    return code;
  }

#define COMP_GEN_II(CMPINST)                                         \
do {                                                                 \
    mov(eax, dword [ecx + off0]);                                    \
    cmp(eax, dword [ecx + off1]);                                    \
    CMPINST(al);						     \
    mov(ah, 0);							     \
} while(0)

#define COMP_GEN_IF(CMPINST)                                         \
do {                                                                 \
    cvtsi2sd(xmm0, ptr [ecx + off0]);                                \
    xor(eax, eax);					             \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST(al);						     \
} while(0)

#define COMP_GEN_FI(CMPINST)                                         \
do {                                                                 \
    sub(esp, 8); 					             \
    movsd(qword [esp], xmm1);   				     \
    movsd(xmm0, ptr [ecx + off0]);				     \
    cvtsi2sd(xmm1, ptr [ecx + off1]);                                \
    xor(eax, eax);					             \
    comisd(xmm0, xmm1);     			                     \
    CMPINST(al);						     \
    movsd(xmm1, ptr [esp]);					     \
    add(esp, 8);						     \
} while(0)

#define COMP_GEN_FF(CMPINST)                                         \
do {                                                                 \
    movsd(xmm0, dword [ecx + off0]);                                 \
    xor(eax, eax);					             \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST(al);						     \
} while(0)
    
#define COMP_GEN(CMPINSTI, CMPINSTF)				     \
do {                                                                 \
    int regno = GETARG_A(**ppc);                                     \
    const Xbyak::uint32 off0 = regno * sizeof(mrb_value);            \
    const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);             \
    mov(eax, dword [ecx + off0 + 4]); /* Get type tag */             \
    gen_type_guard(mrb, (enum mrb_vtype)mrb_type(regs[regno]), *ppc);	\
    mov(eax, dword [ecx + off1 + 4]); /* Get type tag */             \
    gen_type_guard(mrb, (enum mrb_vtype)mrb_type(regs[regno + 1]), *ppc); \
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
    cwde();                                                          \
    add(eax, eax);                                                   \
    add(eax, 0xfff00001);                                            \
    mov(dword [ecx + off0 + 4], eax);                                \
    mov(dword [ecx + off0], 1);                                      \
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
    emit_getupvar(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const Xbyak::uint32 uppos = GETARG_C(**ppc);
    const Xbyak::uint32 idxpos = GETARG_B(**ppc);
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 3 * sizeof(void *);

    push(ecx);
    push(ebx);
    push(idxpos);
    push(uppos);
    push((Xbyak::uint32)mrb);
    call((void *)mrb_uvget);
    add(sp, argsize);
    pop(ebx);
    pop(ecx);
    mov(dword [ecx + dstoff], eax);
    mov(dword [ecx + dstoff + 4], edx);

    return code;
  }

  const void *
    emit_setupvar(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    const Xbyak::uint32 uppos = GETARG_C(**ppc);
    const Xbyak::uint32 idxpos = GETARG_B(**ppc);
    const Xbyak::uint32 valoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 5 * sizeof(void *);

    push(ecx);
    push(ebx);
    mov(eax, dword [ecx + valoff + 4]);
    push(eax);
    mov(eax, dword [ecx + valoff]);
    push(eax);
    push(idxpos);
    push(uppos);
    push((Xbyak::uint32)mrb);
    call((void *)mrb_uvset);
    add(sp, argsize);
    pop(ebx);
    pop(ecx);

    return code;
  }

  const void *
    emit_jmp(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
  {
    const void *code = getCurr();
    gen_jmp(mrb, irep,  *ppc, *ppc + GETARG_sBx(**ppc));
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
      gen_bool_guard(mrb, 1, *ppc + 1);
      gen_jmp(mrb, irep, *ppc, *ppc + GETARG_sBx(**ppc));
    }
    else {
      gen_bool_guard(mrb, 0, *ppc + GETARG_sBx(**ppc));
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
      gen_bool_guard(mrb, 0, *ppc + 1);
      gen_jmp(mrb, irep, *ppc, *ppc + GETARG_sBx(**ppc));
    }
    else {
      gen_bool_guard(mrb, 1, *ppc + GETARG_sBx(**ppc));
    }

    return code;
  }
};

#endif  /* MRUBY_JITCODE_H */

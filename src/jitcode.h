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

void *mrbjit_exec_send_c(mrb_state *, mrbjit_vmstatus *, 
		      struct RProc *, struct RClass *);

void *mrbjit_exec_send_mruby(mrb_state *, mrbjit_vmstatus *, 
		      struct RProc *, struct RClass *);
void *mrbjit_exec_enter(mrb_state *, mrbjit_vmstatus *);
void *mrbjit_exec_return(mrb_state *, mrbjit_vmstatus *);
void *mrbjit_exec_call(mrb_state *, mrbjit_vmstatus *);
} /* extern "C" */

#define OffsetOf(s_type, field) ((size_t) &((s_type *)0)->field) 
#define CALL_MAXARGS 127

/* Regs Map                               *
 * ecx   -- pointer to regs               *
 * ebx   -- pointer to status                 */
class MRBJitCode: public Xbyak::CodeGenerator {

 public:

 MRBJitCode():
  CodeGenerator(1024 * 1024)
  {
  }

  const void *
    gen_entry(mrb_state *mrb, mrbjit_vmstatus *status) 
  {
    const void* func_ptr = getCurr();
    return func_ptr;
  }

  void 
    gen_exit(mrb_code *pc, int is_clr_rc, int is_clr_exitpos)
  {
    inLocalLabel();
    L(".exitlab");
    if (pc) {
      mov(edx, dword [ebx + OffsetOf(mrbjit_vmstatus, pc)]);
      mov(dword [edx], (Xbyak::uint32)pc);
    }
    if (is_clr_rc) {
      xor(eax, eax);
    }
    if (is_clr_exitpos) {
      xor(edx, edx);
    }
    else {
      //mov(edx, (Xbyak::uint32)exit_ptr);
      mov(edx, ".exitlab");
    }
    ret();
    outLocalLabel();
  }
  
  void 
    gen_jump_block(void *entry) 
  {
    jmp(entry);
  }

  void 
    gen_jmp_patch(void *dst, void *target) 
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    jmp(target);
    setSize(cursize);
  }

  void 
    gen_jmp(mrb_state *mrb, mrbjit_vmstatus *status, mrb_code *curpc, mrb_code *newpc)
  {
    mrbjit_code_info *newci;
    mrb_irep *irep = *status->irep;
    int n = ISEQ_OFFSET_OF(newpc);
    if (irep->ilen < NO_INLINE_METHOD_LEN || irep->jit_inlinep) {
      newci = mrbjit_search_codeinfo_prev(irep->jit_entry_tab + n, 
					  curpc, mrb->c->ci->pc);
    }
    else {
      newci = mrbjit_search_codeinfo_prev(irep->jit_entry_tab + n, curpc, NULL);
      mrb->compile_info.nest_level = 0;
    }
    if (newci) {
      if (newci->used > 0) {
	jmp((void *)newci->entry);
      }
      else {
	newci->entry = (void *(*)())getCurr();
	gen_exit(newpc, 1, 0);
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
    gen_exit(pc, 1, 0);

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
    gen_exit(pc, 1, 0);

    L("@@");
  }

  /* Check current object blong to class. Difference of type guard is 
   this guard chaeck obj->c when v is normal object.
   input EAX Pointer to  checkee object
     destroy EAX
  */
  void 
    gen_class_guard(mrb_state *mrb, mrb_value v, mrb_code *pc)
  {
    enum mrb_vtype tt;
    tt = (enum mrb_vtype)mrb_type(v);
    if (tt == MRB_TT_FLOAT) {
      cmp(dword [eax + 4], 0xfff00000);
      jb("@f");
    } 
    else {
      cmp(dword [eax + 4], 0xfff00000 | tt);
      jz("@f");
    }

    /* Guard fail exit code */
    gen_exit(pc, 1, 0);

    L("@@");
    /* Import from class.h */
    switch (tt) {
    case MRB_TT_FALSE:
    case MRB_TT_TRUE:
    case MRB_TT_SYMBOL:
    case MRB_TT_FIXNUM:
    case MRB_TT_FLOAT:
      /* DO NOTHING */
      break;

    default:
      mov(eax, dword [eax]);
      mov(eax, dword [eax + OffsetOf(struct RBasic, c)]);
      cmp(eax, (int)mrb_object(v)->c);
      jz("@f");
      /* Guard fail exit code */
      gen_exit(pc, 1, 0);

      L("@@");
      break;
    }
  }
  
  void
    gen_lvar_get(const Xbyak::Mmx& dst, int no, mrbjit_code_info *coi)
  {
    
  }

  const void *
    emit_nop(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    return code;
  }

  const void *
    emit_move(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    movsd(xmm0, ptr [ecx + srcoff]);
    movsd(ptr [ecx + dstoff], xmm0);
    return code;
  }

  const void *
    emit_loadl(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_irep *irep = *status->irep;
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 srcoff = GETARG_Bx(**ppc) * sizeof(mrb_value);
    mov(eax, (Xbyak::uint32)irep->pool + srcoff);
    movsd(xmm0, ptr [eax]);
    movsd(ptr [ecx + dstoff], xmm0);

    return code;
  }

  const void *
    emit_loadi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 src = GETARG_sBx(**ppc);
    mov(eax, src);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_FIXNUM);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_loadsym(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrb_irep *irep = *status->irep;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_Bx(**ppc);
    const Xbyak::uint32 src = (Xbyak::uint32)irep->syms[srcoff];
    mov(eax, src);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_SYMBOL);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_loadself(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);

    movsd(xmm0, ptr [ecx]);
    movsd(ptr [ecx + dstoff], xmm0);
    return code;
  }

  const void *
    emit_loadt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mov(eax, 1);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_TRUE);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_loadf(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mov(eax, 1);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_FALSE);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

  const void *
    emit_getiv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_sym id = irep->syms[idpos];
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrb_value self = mrb->c->stack[0];
    const int ivoff = mrbjit_iv_off(mrb, self, id);

    if (ivoff < 0) {
      return NULL;
    }
    mov(eax, ecx);
    gen_class_guard(mrb, self, *ppc);

    mov(eax, dword [ecx]);
    mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
    mov(eax, dword [eax]);
    movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);
    movsd(ptr [ecx + dstoff], xmm0);

    return code;
  }

  const void *
    emit_setiv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 srcoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int idpos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_sym id = irep->syms[idpos];
    mrb_value self = mrb->c->stack[0];
    const int ivoff = mrbjit_iv_off(mrb, self, id);

    if (ivoff < 0) {
      return NULL;
    }
    mov(eax, ecx);
    gen_class_guard(mrb, self, *ppc);

    mov(eax, dword [ecx]);
    mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
    mov(eax, dword [eax]);
    movsd(xmm0, ptr [ecx + srcoff]);
    movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);

    return code;
  }

  const void *
    emit_getcv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 2 * sizeof(void *);
    mrb_irep *irep = *status->irep;

    push(ecx);
    push(ebx);
    push((Xbyak::uint32)irep->syms[idpos]);
    push(esi);
    call((void *)mrb_vm_cv_get);
    add(esp, argsize);
    pop(ebx);
    pop(ecx);
    mov(dword [ecx + dstoff], eax);
    mov(dword [ecx + dstoff + 4], edx);

    return code;
  }

  const void *
    emit_setcv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    const Xbyak::uint32 srcoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 4 * sizeof(void *);
    mrb_irep *irep = *status->irep;

    push(ecx);
    push(ebx);
    mov(eax, dword [ecx + srcoff + 4]);
    push(eax);
    mov(eax, dword [ecx + srcoff]);
    push(eax);
    push((Xbyak::uint32)irep->syms[idpos]);
    push(esi);
    call((void *)mrb_vm_cv_set);
    add(esp, argsize);
    pop(ebx);
    pop(ecx);

    return code;
  }

  const void *
    emit_getconst(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int sympos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    const mrb_value v = mrb_vm_const_get(mrb, irep->syms[sympos]);

    mov(dword [ecx + dstoff], v.value.i);
    mov(dword [ecx + dstoff + 4], v.ttt);
    
    return code;
  }

  const void *
    emit_loadnil(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    xor(eax, eax);
    mov(dword [ecx + dstoff], eax);
    mov(eax, 0xfff00000 | MRB_TT_FALSE);
    mov(dword [ecx + dstoff + 4], eax);

    return code;
  }

#define CALL_CFUNC_BEGIN                                             \
  do {                                                               \
    push(ecx);                                                       \
    push(ebx);                                                       \
  } while (0)

#define CALL_CFUNC_STATUS(func_name, auxargs)			     \
  do {                                                               \
    push(ebx);                                                       \
\
    /* Update pc */                                                  \
    mov(eax, dword [ebx + OffsetOf(mrbjit_vmstatus, pc)]);           \
    mov(dword [eax], (Xbyak::uint32)(*status->pc));                  \
\
    push(esi);                                                       \
    call((void *)func_name);                                         \
    add(esp, (auxargs + 2) * 4);				     \
    pop(ebx);                                                        \
    pop(ecx);                                                        \
\
    test(eax, eax);					             \
    jz("@f");                                                        \
    gen_exit(NULL, 0, 0);				             \
    L("@@");                                                         \
  }while (0)

  const void *
    emit_send(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_code *pc = *status->pc;
    mrb_value *regs = *status->regs;
    mrb_sym *syms = *status->syms;
    mrb_irep *irep = *status->irep;
    int i = *pc;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
    struct RProc *m;
    mrb_value prim;
    struct RClass *c;
    const void *code = getCurr();
    mrb_value recv;
    mrb_sym mid = syms[GETARG_B(i)];

    if (GETARG_C(i) == CALL_MAXARGS) {
      return NULL;
    }

    recv = regs[a];
    c = mrb_class(mrb, recv);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (!m) {
      return NULL;
    }

    lea(eax, ptr [ecx + a * sizeof(mrb_value)]);
    gen_class_guard(mrb, recv, pc);

    if (GET_OPCODE(i) != OP_SENDB) {
      //SET_NIL_VALUE(regs[a+n+1]);
      int dstoff = (a + n + 1) * sizeof(mrb_value);
      xor(eax, eax);
      mov(dword [ecx + dstoff], eax);
      mov(eax, 0xfff00000 | MRB_TT_FALSE);
      mov(dword [ecx + dstoff + 4], eax);
    }

    if (MRB_PROC_CFUNC_P(m)) {
      prim = mrb_obj_iv_get(mrb, (struct RObject *)c, mid);
      mrb->vmstatus = status;
      if (mrb_type(prim) == MRB_TT_PROC) {
	mrb_value res = mrb_proc_ptr(prim)->body.func(mrb, prim);
	if (!mrb_nil_p(res)) {
	  return code;
	}
      }

      //puts(mrb_sym2name(mrb, mid)); // for tuning
      CALL_CFUNC_BEGIN;
      mov(eax, (Xbyak::uint32)c);
      push(eax);
      mov(eax, (Xbyak::uint32)m);
      push(eax);
      CALL_CFUNC_STATUS(mrbjit_exec_send_c, 2);
    }
    else {
      int ioff;
      int toff;
      mrbjit_codetab *ctab;

      CALL_CFUNC_BEGIN;
      mov(eax, (Xbyak::uint32)c);
      push(eax);
      mov(eax, (Xbyak::uint32)m);
      push(eax);
      CALL_CFUNC_STATUS(mrbjit_exec_send_mruby, 2);

      mov(eax, dword [ebx + OffsetOf(mrbjit_vmstatus, regs)]);
      mov(ecx, dword [eax]);

      //ci->jit_entry = (irep->jit_entry_tab + ioff)->body[0].entry;
      mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
      mov(eax, dword [eax + OffsetOf(mrb_context, ci)]);
      lea(eax, dword [eax + OffsetOf(mrb_callinfo, jit_entry)]);
      ioff = ISEQ_OFFSET_OF(pc);
      toff = coi - (irep->jit_entry_tab + ioff)->body;

      /* Check and grow code table */
      ctab = (irep->jit_entry_tab + ioff + 1);
      if (ctab->size <= toff) {
	int oldsize;
	int j;

	oldsize = ctab->size;
	ctab->size = oldsize + (oldsize >> 1) + 2;
	ctab->body = (mrbjit_code_info *)mrb_realloc(mrb, ctab->body, 
				sizeof(mrbjit_code_info) * ctab->size);
	for (j = oldsize; j < ctab->size; j++) {
	  ctab->body[j].used = 0;
	}
      }

      /* This is unused entry, but right.Because no other pathes */
      mov(edx, (Xbyak::uint32)ctab);
      mov(edx, dword [edx + OffsetOf(mrbjit_codetab, body)]);
      mov(edx, dword [edx 
		      + toff * sizeof(mrbjit_code_info)
		      + OffsetOf(mrbjit_code_info, entry)]);

      //printf("%d ", toff);
      mov(dword [eax], edx);
    }

    return code;
  }

  const void *
    emit_call(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();
    mrb_callinfo *ci = mrb->c->ci;
    mrb_value recv = mrb->c->stack[0];
    struct RProc *m = mrb_proc_ptr(recv);
    
    if (ci->argc < 0) {
      return NULL;
    }

    if (MRB_PROC_CFUNC_P(m)) {
      return NULL;
    }

    if (m->body.irep == NULL) {
      return NULL;
    }

    if (ci->argc < 0) {
      return NULL;
    }

    mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
    mov(eax, dword [eax + OffsetOf(mrb_context, stack)]);
    mov(eax, dword [eax + OffsetOf(mrb_value, value.p)]);
    mov(eax, dword [eax + OffsetOf(struct RProc, body.irep)]);
    mov(eax, dword [eax + OffsetOf(mrb_irep, jit_top_entry)]);
    test(eax, eax);
    jnz("@f");
    gen_exit(*status->pc, 1, 1);
    L("@@");
    push(eax);
    CALL_CFUNC_BEGIN;
    CALL_CFUNC_STATUS(mrbjit_exec_call, 0);
    pop(eax);
    jmp(eax);

    return code;
  }

  const void *
    emit_enter(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();
    mrb_code *pc = *status->pc;
    mrb_code i = *pc;
    /* Ax             arg setup according to flags (24=5:5:1:5:5:1:1) */
    /* number of optional arguments times OP_JMP should follow */
    int ax = GETARG_Ax(i);
    /* int m1 = (ax>>18)&0x1f; */
    int o  = (ax>>13)&0x1f;
    int r  = (ax>>12)&0x1;
    int m2 = (ax>>7)&0x1f;

    if (o != 0 || r != 0 || m2 != 0) {
      CALL_CFUNC_BEGIN;
      CALL_CFUNC_STATUS(mrbjit_exec_enter, 0);
    }

    return code;
  }

  const void *
    emit_return(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();

    /* Set return address from callinfo */
    mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
    mov(eax, dword [eax + OffsetOf(mrb_context, ci)]);
    mov(eax, dword [eax + OffsetOf(mrb_callinfo, jit_entry)]);
    test(eax, eax);
    jnz("@f");
    gen_exit(*status->pc, 1, 0);
    L("@@");
    push(eax);
    
    push(ecx);
    push(ebx);

    push(ebx);
    /* Update pc */
    mov(eax, dword [ebx + OffsetOf(mrbjit_vmstatus, pc)]);
    mov(dword [eax], (Xbyak::uint32)(*status->pc));

    push(esi);
    call((void *)mrbjit_exec_return);
    add(esp, 2 * 4);
    pop(ebx);
    pop(ecx);

    test(eax, eax);
    jz("@f");
    pop(edx);			/* pop return address from callinfo */
    gen_exit(NULL, 0, 0);
    L("@@");

    mov(eax, dword [ebx + OffsetOf(mrbjit_vmstatus, regs)]);
    mov(ecx, dword [eax]);

    ret();

    return code;
  }

  const void *
    emit_return_inline(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    const void *code = getCurr();
    CALL_CFUNC_BEGIN;
    CALL_CFUNC_STATUS(mrbjit_exec_return, 0);

    mov(eax, dword [ebx + OffsetOf(mrbjit_vmstatus, regs)]);
    mov(ecx, dword [eax]);

    return code;
  }

#define OVERFLOW_CHECK_GEN(AINSTF)                                      \
    jno("@f");                                                          \
    mov(eax, dword [ecx + reg0off]);                                    \
    cvtsi2sd(xmm0, eax);                                                \
    mov(eax, dword [ecx + reg1off]);                                    \
    cvtsi2sd(xmm1, eax);                                                \
    AINSTF(xmm0, xmm1);                                                 \
    movsd(dword [ecx + reg0off], xmm0);                                 \
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
    else if ((r0type == MRB_TT_FLOAT || r0type == MRB_TT_FIXNUM) &&     \
             (r1type == MRB_TT_FLOAT || r1type == MRB_TT_FIXNUM)) {	\
      if (r0type == MRB_TT_FIXNUM) {                                    \
        cvtsi2sd(xmm0, dword [ecx + reg0off]);                          \
      }                                                                 \
      else {                                                            \
        movsd(xmm0, dword [ecx + reg0off]);                             \
      }                                                                 \
\
      if (r1type == MRB_TT_FIXNUM) {                                    \
        cvtsi2sd(xmm1, dword [ecx + reg1off]);                          \
      }                                                                 \
      else {                                                            \
        movsd(xmm1, dword [ecx + reg1off]);                             \
      }                                                                 \
\
      AINSTF(xmm0, xmm1);				                \
      movsd(ptr [ecx + reg0off], xmm0);                                 \
    }                                                                   \
    else {                                                              \
      gen_exit(*ppc, 1, 0);						\
    }                                                                   \
} while(0)

  const void *
    emit_add(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(add, addsd);
    return code;
  }

  const void *
    emit_sub(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(sub, subsd);
    return code;
  }

  const void *
    emit_mul(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(imul, mulsd);
    return code;
  }

  const void *
    emit_div(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int reg0pos = GETARG_A(**ppc);
    int reg1pos = reg0pos + 1;
    const Xbyak::uint32 reg0off = reg0pos * sizeof(mrb_value);
    const Xbyak::uint32 reg1off = reg1pos * sizeof(mrb_value);
    enum mrb_vtype r0type = (enum mrb_vtype) mrb_type(regs[reg0pos]);
    enum mrb_vtype r1type = (enum mrb_vtype) mrb_type(regs[reg1pos]);

    mov(eax, dword [ecx + reg0off + 4]); /* Get type tag */
    gen_type_guard(mrb, r0type, *ppc);
    mov(eax, dword [ecx + reg1off + 4]); /* Get type tag */
    gen_type_guard(mrb, r1type, *ppc);

    if (r0type == MRB_TT_FIXNUM) {
      cvtsi2sd(xmm0, dword [ecx + reg0off]);
    }
    else {
      movsd(xmm0, dword [ecx + reg0off]);
    }

    if (r1type == MRB_TT_FIXNUM) {
      cvtsi2sd(xmm1, dword [ecx + reg1off]);
    }
    else {
      movsd(xmm1, dword [ecx + reg1off]);
    }

    divsd(xmm0, xmm1);
    movsd(ptr [ecx + reg0off], xmm0);

    return code;
  }

#define OVERFLOW_CHECK_I_GEN(AINSTF)                                    \
    jno("@f");                                                          \
    mov(eax, dword [ecx + off]);                                        \
    cvtsi2sd(xmm0, eax);                                                \
    mov(eax, y);                                                        \
    cvtsi2sd(xmm1, eax);                                                \
    AINSTF(xmm0, xmm1);                                                 \
    movsd(dword [ecx + off], xmm0);                                     \
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
      movsd(xmm0, ptr [ecx + off]);                                     \
      mov(eax, y);                                                      \
      cvtsi2sd(xmm1, eax);                                              \
      AINSTF(xmm0, xmm1);                                               \
      movsd(ptr [ecx + off], xmm0);                                     \
    }                                                                   \
    else {                                                              \
      gen_exit(*ppc, 1, 0);						\
    }                                                                   \
} while(0)
    
  const void *
    emit_addi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_I_GEN(add, addsd);
    return code;
  }

  const void *
    emit_subi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
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
    movsd(xmm0, ptr [ecx + off0]);				     \
    cvtsi2sd(xmm1, ptr [ecx + off1]);                                \
    xor(eax, eax);					             \
    comisd(xmm0, xmm1);     			                     \
    CMPINST(al);						     \
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
    emit_eq(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    COMP_GEN(setz, setz);

    return code;
  }

  const void *
    emit_lt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    COMP_GEN(setl, setb);

    return code;
  }

  const void *
    emit_le(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    COMP_GEN(setle, setbe);

    return code;
  }

  const void *
    emit_gt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    COMP_GEN(setg, seta);

    return code;
  }

  const void *
    emit_ge(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    COMP_GEN(setge, setae);

    return code;
  }

  const void *
    emit_getupvar(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 uppos = GETARG_C(**ppc);
    const Xbyak::uint32 idxpos = GETARG_B(**ppc);
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 3 * sizeof(void *);

    push(ecx);
    push(ebx);
    push(idxpos);
    push(uppos);
    push(esi);
    call((void *)mrb_uvget);
    add(esp, argsize);
    pop(ebx);
    pop(ecx);
    mov(dword [ecx + dstoff], eax);
    mov(dword [ecx + dstoff + 4], edx);

    return code;
  }

  const void *
    emit_setupvar(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
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
    push(esi);
    call((void *)mrb_uvset);
    add(esp, argsize);
    pop(ebx);
    pop(ecx);

    return code;
  }

  const void *
    emit_jmp(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    gen_jmp(mrb, status, *ppc, *ppc + GETARG_sBx(**ppc));
    return code;
  }

  const void *
    emit_jmpif(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int cond = GETARG_A(**ppc);
    const Xbyak::uint32 coff =  cond * sizeof(mrb_value);
    
    mov(eax, ptr [ecx + coff + 4]);
    if (mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 1, *ppc + 1);
      gen_jmp(mrb, status, *ppc, *ppc + GETARG_sBx(**ppc));
    }
    else {
      gen_bool_guard(mrb, 0, *ppc + GETARG_sBx(**ppc));
    }

    return code;
  }

  const void *
    emit_jmpnot(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int cond = GETARG_A(**ppc);
    const Xbyak::uint32 coff =  cond * sizeof(mrb_value);
    
    mov(eax, ptr [ecx + coff + 4]);
    if (!mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 0, *ppc + 1);
      gen_jmp(mrb, status, *ppc, *ppc + GETARG_sBx(**ppc));
    }
    else {
      gen_bool_guard(mrb, 1, *ppc + GETARG_sBx(**ppc));
    }

    return code;
  }

  const void *
    emit_lambda(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int kind = GETARG_c(**ppc);
    const int lno = GETARG_b(**ppc);
    const int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrb_irep *mirb = mrb->irep[(*(status->irep))->idx + lno];

    if (mirb->proc_obj) {
      mov(eax, (Xbyak::uint32)mirb->proc_obj);
      mov(dword [ecx + dstoff], eax);
      mov(eax, 0xfff00000 | MRB_TT_PROC);
      mov(dword [ecx + dstoff + 4], eax);
    }
    else {
      gen_exit(*ppc, 1, 0);
    }

    return code;
  }

  /* primitive methodes */
  mrb_value mrbjit_prim_num_cmp_impl(mrb_state *mrb, mrb_value proc);
  mrb_value mrbjit_prim_fix_succ_impl(mrb_state *mrb, mrb_value proc);
  mrb_value mrbjit_prim_obj_not_equal_m_impl(mrb_state *mrb, mrb_value proc);
  mrb_value mrbjit_prim_ary_aget_impl(mrb_state *mrb, mrb_value proc);
};

#endif  /* MRUBY_JITCODE_H */

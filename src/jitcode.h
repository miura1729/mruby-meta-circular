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
#include "mruby/range.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/hash.h"
#include "mruby/class.h"
#include "mruby/jit.h"

void *mrbjit_exec_send_c(mrb_state *, mrbjit_vmstatus *, 
		      struct RProc *, struct RClass *);
void *mrbjit_exec_extend_callinfo(mrb_state *, mrb_context *, int);

void *mrbjit_exec_send_mruby(mrb_state *, mrbjit_vmstatus *, 
		      struct RProc *, struct RClass *);
void *mrbjit_exec_enter(mrb_state *, mrbjit_vmstatus *);
void *mrbjit_exec_return(mrb_state *, mrbjit_vmstatus *);
void *mrbjit_exec_return_fast(mrb_state *, mrbjit_vmstatus *);
void *mrbjit_exec_call(mrb_state *, mrbjit_vmstatus *);
void disasm_once(mrb_state *, mrb_irep *, mrb_code);
} /* extern "C" */

#define OffsetOf(s_type, field) ((size_t) &((s_type *)0)->field) 
#define VMSOffsetOf(field) (((intptr_t)status->field) - ((intptr_t)status->pc))
#define CALL_MAXARGS 127

/* Regs Map                               *
 * ecx   -- pointer to regs               *
 * ebx   -- pointer to status->pc         *
 * esi   -- pointer to mrb                *
 * edi   -- pointer to mrb->c             */
class MRBJitCode: public Xbyak::CodeGenerator {

  void *addr_call_extend_callinfo;
  void *addr_call_stack_extend;

 public:

 MRBJitCode():
  CodeGenerator(1024 * 1024)
  {
    addr_call_extend_callinfo = NULL;
    addr_call_stack_extend = NULL;
  }

  const void
    set_entry(const void * entry)
  {
    const unsigned char *code = getCode();
    size_t entsize = (unsigned char *)entry - code;
    setSize(entsize);
  }

  const void *
    gen_entry(mrb_state *mrb, mrbjit_vmstatus *status) 
  {
    const void* func_ptr = getCurr();
    return func_ptr;
  }

  void 
    gen_exit(mrb_code *pc, int is_clr_rc, int is_clr_exitpos, mrbjit_vmstatus *status)
  {
    inLocalLabel();
    L(".exitlab");
    if (pc) {
      mov(dword [ebx + VMSOffsetOf(pc)], (Xbyak::uint32)pc);
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
    gen_exit_patch(void *dst, mrb_code *pc, mrbjit_vmstatus *status)
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    gen_exit(pc, 1, 0, status);
    setSize(cursize);
  }

  void 
    gen_align(unsigned align)
  {
    const unsigned char *code = getCurr();
    unsigned padsize = (((size_t)code) & (align - 1));
    unsigned i;

    padsize = (align - padsize) & (align - 1);
    for (i = 0; i < padsize; i++) {
      nop();
    }
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
      /*      else {
	newci->entry = (void *(*)())getCurr();
	gen_exit(newpc, 1, 0, status);
	}*/
      mrb->compile_info.code_base = NULL;
    }
  }

  void 
    gen_type_guard(mrb_state *mrb, int regpos, mrbjit_vmstatus *status, mrb_code *pc, mrbjit_code_info *coi)
  {
    enum mrb_vtype tt = (enum mrb_vtype) mrb_type((*status->regs)[regpos]);
    mrbjit_reginfo *rinfo = &coi->reginfo[regpos];

    if (rinfo->type == tt) {
      return;
    }

    mov(eax, dword [ecx + regpos * sizeof(mrb_value) + 4]); /* Get type tag */
    rinfo->type = tt;
    rinfo->klass = mrb_class(mrb, (*status->regs)[regpos]);
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
    gen_exit(pc, 1, 0, status);

    L("@@");
  }

  /*
   input EAX Pointer to tested boolean
  */
  void
    gen_bool_guard(mrb_state *mrb, int b, mrb_code *pc, 
		   mrbjit_vmstatus *status, mrbjit_reginfo *rinfo)
  {
    if (rinfo->constp) {
      if (b && rinfo->type != MRB_TT_FALSE) {
	return;
      }
      if (!b && rinfo->type == MRB_TT_FALSE) {
	return;
      }
    }

    cmp(eax, 0xfff00001);
    if (b) {
      jnz("@f");
    } 
    else {
      jz("@f");
    }

    /* Guard fail exit code */
    gen_exit(pc, 1, 0, status);

    L("@@");
  }

  /* Check current object blong to class. Difference of type guard is 
   this guard chaeck obj->c when v is normal object.
     destroy EAX
  */
  void 
    gen_class_guard(mrb_state *mrb, int regpos, mrbjit_vmstatus *status, mrb_code *pc, mrbjit_code_info *coi, struct RClass *c)
  {
    enum mrb_vtype tt;
    mrb_value v = (*status->regs)[regpos];
    mrbjit_reginfo *rinfo = &coi->reginfo[regpos];

    tt = (enum mrb_vtype)mrb_type(v);

    if (rinfo->type != tt) {

      rinfo->type = tt;

      mov(eax, ptr [ecx + regpos * sizeof(mrb_value) + 4]);

      if (tt == MRB_TT_FLOAT) {
	cmp(eax, 0xfff00000);
	jb("@f");
      }
      else {
	cmp(eax, 0xfff00000 | tt);
	jz("@f");
      }

      /* Guard fail exit code */
      gen_exit(pc, 1, 0, status);

      L("@@");
    }

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
      {
	if (c == NULL) {
	  c = mrb_object(v)->c;
	}
	if (rinfo->klass == c) {
	  return;
	}
	rinfo->klass = c;
	mov(eax, dword [ecx + regpos * sizeof(mrb_value)]);
	mov(eax, dword [eax + OffsetOf(struct RBasic, c)]);
	cmp(eax, (int)c);
	jz("@f");
	/* Guard fail exit code */
	gen_exit(pc, 1, 0, status);

	L("@@");
      }
      break;
    }
  }
  
  void
    gen_lvar_get(const Xbyak::Mmx& dst, int no, mrbjit_code_info *coi)
  {
    
  }

  void 
    gen_set_jit_entry(mrb_state *mrb, mrb_code *pc, mrbjit_code_info *coi, mrb_irep *irep)
  {
    int ioff;
    int toff;
    mrbjit_codetab *ctab;

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

    //ci->jit_entry = (irep->jit_entry_tab + ioff)->body[0].entry;
    /* edi must point current context  */
    mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);

    mov(edx, dword [edx 
		    + toff * sizeof(mrbjit_code_info)
		    + OffsetOf(mrbjit_code_info, entry)]);

    //printf("%d ", toff);
    mov(dword [eax + OffsetOf(mrb_callinfo, jit_entry)], edx);
  }

  void
    gen_call_fetch_hook(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    push(ecx);
    push(ebx);
    push(ecx);
    //    mov(eax, dword [ebx + VMSOffsetOf(pc)]);
    mov(eax, (Xbyak::uint32)(*(status->pc)));
    push(eax);
    mov(eax, (Xbyak::uint32)(*(status->irep)));
    push(eax);
    push(esi);
    call((void *)mrb->code_fetch_hook);
    add(esp, sizeof(void *) * 4);
    pop(ebx);
    pop(ecx);
  }

  void 
    gen_send_mruby(mrb_state *mrb, struct RProc *m, mrb_value recv, 
		   mrbjit_vmstatus *status, mrb_code *pc, mrbjit_code_info *coi)
  {
    int callee_nregs;
    mrb_irep *irep = *status->irep;
    int i = *pc;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
    struct RClass *c = mrb_class(mrb, recv);
    int is_block_call = (m->body.irep->ilen <= 1);

    callee_nregs = m->body.irep->nregs;

    /* Reg map */
    /*    old ci  edx */
    /*    tmp  eax */
    mov(edx, dword [edi + OffsetOf(mrb_context, ci)]);

    cmp(edx, dword [edi + OffsetOf(mrb_context, ciend)]);
    jb("@f");

    if (addr_call_extend_callinfo == NULL) {
      mov(eax, "@f");
      push(eax);

      addr_call_extend_callinfo = (void *)getCurr();

      /* extend cfunction */
      push(edx);
      push(ebx);
      mov(eax, dword [edi + OffsetOf(mrb_context, cibase)]);
      sub(eax, edx);
      neg(eax);
      shr(eax, 6);		/* sizeof mrb_callinfo */
      push(eax);
      mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
      push(eax);
      push(esi);
      call((void *) mrbjit_exec_extend_callinfo);
      add(esp, 3 * sizeof(void *));
      pop(ebx);
      pop(edx);
      mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
      ret();
    }
    else {
      call(addr_call_extend_callinfo);
    }

    L("@@");
    /*    ci  edi */
    /*    tmp  edx */
    /*    tmp  eax */
    add(dword [edi + OffsetOf(mrb_context, ci)], (Xbyak::uint32)sizeof(mrb_callinfo));
    mov(edi, dword [edi + OffsetOf(mrb_context, ci)]);

    mov(eax, dword [edx + OffsetOf(mrb_callinfo, eidx)]);
    mov(dword [edi + OffsetOf(mrb_callinfo, eidx)], eax);
    mov(eax, dword [edx + OffsetOf(mrb_callinfo, ridx)]);
    mov(dword [edi + OffsetOf(mrb_callinfo, ridx)], eax);

    xor(eax, eax);
    mov(dword [edi + OffsetOf(mrb_callinfo, env)], eax);
    mov(dword [edi + OffsetOf(mrb_callinfo, jit_entry)], eax);
    mov(dword [edi + OffsetOf(mrb_callinfo, err)], eax);

    switch(n) {
    case 0:
      mov(dword [edi + OffsetOf(mrb_callinfo, argc)], eax);
      break;

    case 1:
      inc(eax);
      mov(dword [edi + OffsetOf(mrb_callinfo, argc)], eax);
      break;

    case CALL_MAXARGS:
      dec(eax);
      mov(dword [edi + OffsetOf(mrb_callinfo, argc)], eax);
      break;

    default:
      mov(dword [edi + OffsetOf(mrb_callinfo, argc)], (Xbyak::uint32)n);
      break;
    }

    mov(edx, dword [esi + OffsetOf(mrb_state, c)]);
    mov(eax, dword [edx + OffsetOf(mrb_context, stack)]);
    mov(dword [edi + OffsetOf(mrb_callinfo, stackent)], eax);

    if (c->tt == MRB_TT_ICLASS) {
      mov(dword [edi + OffsetOf(mrb_callinfo, target_class)], 
	  (Xbyak::uint32)c->c);
    }
    else {
      mov(dword [edi + OffsetOf(mrb_callinfo, target_class)], 
	  (Xbyak::uint32)c);
    }

    mov(dword [edi + OffsetOf(mrb_callinfo, pc)], (Xbyak::uint32)(pc + 1));

    if (is_block_call) {
      /* Block call */
      callee_nregs = mrb_proc_ptr(recv)->body.irep->nregs;
    }
    else {
      /* normal call */
      mov(dword [edi + OffsetOf(mrb_callinfo, nregs)], 
	  (Xbyak::uint32)m->body.irep->nregs);

      mov(dword [edi + OffsetOf(mrb_callinfo, proc)], (Xbyak::uint32)m);

      mov(dword [ebx + VMSOffsetOf(irep)], (Xbyak::uint32)m->body.irep);
    }

    mov(eax, (Xbyak::uint32)a);
    mov(dword [edi + OffsetOf(mrb_callinfo, acc)], eax);

    /*  mrb->c   edi  */
    mov(edi, dword [esi + OffsetOf(mrb_state, c)]);
    shl(eax, 3);		/* * sizeof(mrb_value) */
    add(dword [edi + OffsetOf(mrb_context, stack)], eax);
    mov(ecx, dword [edi + OffsetOf(mrb_context, stack)]);

    mov(edx, dword [edi + OffsetOf(mrb_context, stend)]);
    if (m->body.irep->nregs != 0) {
      sub(edx, (Xbyak::uint32)callee_nregs * sizeof(mrb_value));
    }
    cmp(ecx, edx);
    jb("@f");

    if (addr_call_stack_extend == NULL) {
      mov(eax, "@f");
      push(eax);
      if (n == CALL_MAXARGS) {
	mov(edx, (Xbyak::uint32)((callee_nregs < 3) ? 3 : callee_nregs));
	mov(eax, 3);
      }
      else {
	mov(edx, (Xbyak::uint32)callee_nregs);
	mov(eax, (Xbyak::uint32)(mrb->c->ci->argc + 2));
      }

      addr_call_stack_extend = (void *)getCurr();

      push(ebx);
      push(eax);
      push(edx);
      push(esi);
      call((void *) mrbjit_stack_extend);
      add(esp, 3 * sizeof(void *));
      pop(ebx);
      mov(ecx, dword [edi + OffsetOf(mrb_context, stack)]);
      ret();
    }
    else {
      if (n == CALL_MAXARGS) {
	mov(edx, (Xbyak::uint32)((callee_nregs < 3) ? 3 : callee_nregs));
	mov(eax, 3);
      }
      else {
	mov(edx, (Xbyak::uint32)callee_nregs);
	mov(eax, (Xbyak::uint32)(mrb->c->ci->argc + 2));
      }
      call(addr_call_stack_extend);
    }
      
    L("@@");

    mov(dword [ebx + VMSOffsetOf(regs)], ecx);

    gen_set_jit_entry(mrb, pc, coi, irep);

    if (is_block_call) {
      push(ecx);
      push(ebx);

      lea(eax, dword [ebx + VMSOffsetOf(status)]);
      push(eax);
      push(esi);
      call((void *)mrbjit_exec_call);
      add(esp, 2 * sizeof(void *));

      pop(ebx);
      pop(ecx);

      mrb->compile_info.force_compile = 1;
    }
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
    mrbjit_reginfo *sinfo = &coi->reginfo[GETARG_B(**ppc)];
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    *dinfo = *sinfo;

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    mrb_value val = irep->pool[GETARG_Bx(**ppc)];
    dinfo->type = (mrb_vtype)mrb_type(val);
    dinfo->klass = mrb_class(mrb, val);
    dinfo->constp = 1;

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    switch(src) {
    case 0:
      xor(eax, eax);
      mov(dword [ecx + dstoff], eax);
      break;

    case 1:
      xor(eax, eax);
      inc(eax);
      mov(dword [ecx + dstoff], eax);
      break;

    default:
      mov(dword [ecx + dstoff], src);
      break;
    }
    if (dinfo->type != MRB_TT_FIXNUM) {
      mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_FIXNUM);
      dinfo->type = MRB_TT_FIXNUM;
      dinfo->klass = mrb->fixnum_class;
    }
    dinfo->constp = 1;

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_SYMBOL;
    dinfo->klass = mrb->symbol_class;
    dinfo->constp = 1;

    mov(dword [ecx + dstoff], src);
    mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_SYMBOL);

    return code;
  }

  const void *
    emit_loadself(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    mrb_value self = *status->regs[0];
    dinfo->type = (mrb_vtype)mrb_type(self);
    dinfo->klass = mrb->c->ci->target_class;
    dinfo->constp = 1;

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (dinfo->type != MRB_TT_TRUE) {
      xor(eax, eax);
      inc(eax);
      mov(dword [ecx + dstoff], eax);
      mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_TRUE);
      dinfo->type = MRB_TT_TRUE;
      dinfo->klass = mrb->true_class;
      dinfo->constp = 1;
    }

    return code;
  }

  const void *
    emit_loadf(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (dinfo->type != MRB_TT_FALSE) {
      xor(eax, eax);
      inc(eax);
      mov(dword [ecx + dstoff], eax);
      mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_FALSE);
      dinfo->type = MRB_TT_FALSE;
      dinfo->klass = mrb->false_class;
      dinfo->constp = 1;
    }

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;

    if (ivoff < 0) {
      return NULL;
    }

    /* You can not change class of self in Ruby */
    if (mrb_type(self) == MRB_TT_OBJECT) {
      mov(eax, dword [ecx]);
      mov(eax, dword [eax + OffsetOf(struct RObject, segcache)]);
      movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);
    }
    else {
      mov(eax, dword [ecx]);
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      mov(eax, dword [eax]);
      movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);
    }
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
    int ivoff = mrbjit_iv_off(mrb, self, id);

    if (ivoff == -1) {
      /* Normal instance variable set (not defined yet) */
      push(ecx);
      push(ebx);
      mov(eax, ptr [ecx + srcoff + 4]);
      push(eax);
      mov(eax, ptr [ecx + srcoff]);
      push(eax);
      push((Xbyak::uint32)id);
      push(esi);
      call((void *)mrb_vm_iv_set);
      add(esp, sizeof(mrb_state *) + sizeof(Xbyak::uint32) + sizeof(mrb_value));
      pop(ebx);
      pop(ecx);

      return code;
    }

    movsd(xmm0, ptr [ecx + srcoff]);
    mov(eax, dword [ecx]);
    push(ecx);
    push(ebx);
    push(eax);
    push(esi);
    call((void *)mrb_write_barrier);
    add(esp, 4);
    pop(eax);
    pop(ebx);
    pop(ecx);
    if (ivoff == -2) {
      if (mrb_type(self) == MRB_TT_OBJECT) {
	mov(edx, eax);
      }
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      ivoff =  mrb_obj_ptr(self)->iv->last_len;
      inc(dword [eax + OffsetOf(iv_tbl, last_len)]);
      inc(dword [eax + OffsetOf(iv_tbl, size)]);
      mov(eax, dword [eax]);
      movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);
      if (mrb_type(self) == MRB_TT_OBJECT) {
	mov(dword [edx + OffsetOf(struct RObject, segcache)], eax);
      }
      mov(dword [eax + MRB_SEGMENT_SIZE * sizeof(mrb_value) + ivoff * sizeof(mrb_sym)], (Xbyak::uint32)id);
    }
    else {
      if (mrb_type(self) == MRB_TT_OBJECT) {
	mov(eax, dword [eax + OffsetOf(struct RObject, segcache)]);
	movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);
      }
      else {
	mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
	mov(eax, dword [eax]);
	movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);
      }
    }

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;

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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = (mrb_vtype)mrb_type(v);
    dinfo->klass = mrb_class(mrb, v);
    dinfo->constp = 1;

    mov(dword [ecx + dstoff], v.value.i);
    mov(dword [ecx + dstoff + 4], v.value.ttt);
    
    return code;
  }

  const void *
    emit_getmconst(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int sympos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_value *regs = *status->regs;
    const mrb_value v = mrb_const_get(mrb, regs[GETARG_A(**ppc)], irep->syms[sympos]);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = (mrb_vtype)mrb_type(v);
    dinfo->klass = mrb_class(mrb, v);
    dinfo->constp = 1;

    mov(dword [ecx + dstoff], v.value.i);
    mov(dword [ecx + dstoff + 4], v.value.ttt);
    
    return code;
  }

  const void *
    emit_loadnil(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FALSE;
    dinfo->klass = mrb->nil_class;
    dinfo->constp = 1;

    xor(eax, eax);
    mov(dword [ecx + dstoff], eax);
    mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_FALSE);

    return code;
  }

#define CALL_CFUNC_BEGIN                                             \
  do {                                                               \
    push(ecx);                                                       \
    push(ebx);                                                       \
  } while (0)

#define CALL_CFUNC_STATUS(func_name, auxargs)			     \
  do {                                                               \
    lea(eax, dword [ebx + VMSOffsetOf(status)]);                     \
    push(eax);                                                       \
\
    /* Update pc */                                                  \
    mov(dword [ebx + VMSOffsetOf(pc)], (Xbyak::uint32)(*status->pc));\
\
    push(esi);                                                       \
    call((void *)func_name);                                         \
    add(esp, (auxargs + 2) * 4);				     \
    pop(ebx);                                                        \
    pop(ecx);                                                        \
\
    test(eax, eax);					             \
    jz("@f");                                                        \
    gen_exit(NULL, 0, 0, status); 		                     \
    L("@@");                                                         \
  }while (0)

  mrb_sym
    method_check(mrb_state *mrb, struct RProc *m, int opcode)
  {
    mrb_irep *irep;
    mrb_code opiv;

    if (!MRB_PROC_CFUNC_P(m)) {
      irep = m->body.irep;

      if (irep->ilen != 3) {
	return 0;
      }

      opiv = irep->iseq[1];
      if (GET_OPCODE(opiv) == opcode) {
	return irep->syms[GETARG_Bx(opiv)];
      }
    }

    return 0;
  }

  mrb_sym
    is_reader(mrb_state *mrb, struct RProc *m)
  {
    mrb_sym ivid;

    ivid = method_check(mrb, m, OP_GETIV);
    if (ivid) {
      m->body.irep->method_kind = IV_READER;
    }

    return ivid;
  }

  mrb_sym
    is_writer(mrb_state *mrb, struct RProc *m)
  {
    mrb_sym ivid;

    ivid = method_check(mrb, m, OP_SETIV);
    if (ivid) {
      m->body.irep->method_kind = IV_READER;
    }

    return ivid;
  }

  const void *
    emit_send(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_code *pc = *status->pc;
    mrb_value *regs = *status->regs;
    mrb_sym *syms = *status->syms;
    int i = *pc;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
    struct RProc *m;
    mrb_value prim;
    struct RClass *c;
    const void *code = getCurr();
    mrb_value recv;
    mrb_sym mid = syms[GETARG_B(i)];
    mrb_sym ivid;
    //    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(i)];
    
    if (GETARG_C(i) == CALL_MAXARGS) {
      n = 1;
    }

    recv = regs[a];
    c = mrb_class(mrb, recv);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (!m) {
      return NULL;
    }

    gen_class_guard(mrb, a, status, pc, coi, mrb_class(mrb, recv));
    //gen_class_guard(mrb, a, status, pc, coi, c);

    //dinfo->type = MRB_TT_FREE;
    //dinfo->klass = NULL;
    //dinfo->constp = 0;

    if ((ivid = is_reader(mrb, m))) {
      const int ivoff = mrbjit_iv_off(mrb, recv, ivid);

      /* Inline IV reader */
      mov(eax, ptr [ecx + a * sizeof(mrb_value)]);
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      mov(eax, dword [eax]);
      movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);

      // regs[a] = obj;
      movsd(ptr [ecx + a * sizeof(mrb_value)], xmm0);

      return code;
    }

    if ((ivid = is_writer(mrb, m))) {
      const int ivoff = mrbjit_iv_off(mrb, recv, ivid);

      /* Inline IV writer */
      mov(eax, ptr [ecx + a * sizeof(mrb_value)]);
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      mov(eax, dword [eax]);

      // @iv = regs[a];
      movsd(xmm0, ptr [ecx + (a + 1) * sizeof(mrb_value)]);
      movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);

      return code;
    }

    if (GET_OPCODE(i) != OP_SENDB) {
      //SET_NIL_VALUE(regs[a+n+1]);
      int dstoff = (a + n + 1) * sizeof(mrb_value);
      xor(eax, eax);
      mov(dword [ecx + dstoff], eax);
      mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_FALSE);
    }

    prim = mrb_obj_iv_get(mrb, (struct RObject *)c, mid);
    if (mrb_type(prim) == MRB_TT_PROC) {
      mrb_value res = ((mrbjit_prim_func_t)mrb_proc_ptr(prim)->body.func)(mrb, prim, status, coi);
      switch (mrb_type(res)) {
      case MRB_TT_PROC:
	m = mrb_proc_ptr(res);
	break;
	
      case MRB_TT_TRUE:
	return code;
      }
    }

    if (MRB_PROC_CFUNC_P(m)) {
      //mrb_p(mrb, regs[a]);
      //puts(mrb_sym2name(mrb, mid)); // for tuning
      //printf("%x \n", irep);
      CALL_CFUNC_BEGIN;
      push((Xbyak::uint32)c);
      push((Xbyak::uint32)m);
      CALL_CFUNC_STATUS(mrbjit_exec_send_c, 2);
    }
    else {
      gen_send_mruby(mrb, m, recv, status, pc, coi);
    }

    return code;
  }

  const void *
    emit_block_guard(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_irep *irep = *status->irep;
    const void *code = getCurr();

    if (irep->block_lambda) {
      mov(eax, dword [ebx + VMSOffsetOf(irep)]);
      cmp(eax, (Xbyak::uint32)irep);
      jz("@f");
      inLocalLabel();

      L(".exitlab");
      mov(eax, dword [eax + OffsetOf(mrb_irep, iseq)]);
      mov(dword [ebx + VMSOffsetOf(pc)], eax);
      xor(eax, eax);
      mov(edx, ".exitlab");
      ret();

      outLocalLabel();
      L("@@");
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

    mov(eax, dword [ecx + OffsetOf(mrb_value, value.p0)]);
    mov(eax, dword [eax + OffsetOf(struct RProc, body.irep)]);
    mov(eax, dword [eax + OffsetOf(mrb_irep, jit_top_entry)]);
    test(eax, eax);
    push(eax);
    jnz("@f");
    pop(eax);
    gen_exit(*status->pc, 1, 1, status);
    L("@@");

    push(ecx);
    push(ebx);

    lea(eax, dword [ebx + VMSOffsetOf(status)]);
    push(eax);
    push(esi);
    call((void *)mrbjit_exec_call);
    add(esp, 2 * sizeof(void *));

    pop(ebx);
    pop(ecx);
    ret();

    return code;
  }

  const void *
    emit_enter(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code *pc = *status->pc;
    mrb_value *regs = *status->regs;
    mrb_code i = *pc;
    /* Ax             arg setup according to flags (24=5:5:1:5:5:1:1) */
    /* number of optional arguments times OP_JMP should follow */
    int ax = GETARG_Ax(i);
    /* int m1 = (ax>>18)&0x1f; */
    int o  = (ax>>13)&0x1f;
    int r  = (ax>>12)&0x1;
    int m2 = (ax>>7)&0x1f;
    mrbjit_reginfo *selfinfo = &coi->reginfo[0];

    selfinfo->type = (mrb_vtype)mrb_type(regs[0]);
    selfinfo->klass = mrb_class(mrb, regs[0]);
    selfinfo->constp = 1;

    if (mrb->c->ci->argc < 0 || o != 0 || r != 0 || m2 != 0) {
      CALL_CFUNC_BEGIN;
      CALL_CFUNC_STATUS(mrbjit_exec_enter, 0);
    }

    return code;
  }

  const void *
    emit_return(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    struct mrb_context *c = mrb->c;
    mrb_code *pc = *status->pc;
    mrb_code i = *pc;
    int can_use_fast = (c->ci != c->cibase &&
			GETARG_B(i) == OP_R_NORMAL &&
			(c->ci->env == 0 || c->ci->proc->body.irep->shared_lambda));
    int can_inline = (can_use_fast && 
		      (c->ci[-1].eidx == c->ci->eidx) && (c->ci[-1].acc >= 0));
    mrbjit_reginfo *rinfo = &coi->reginfo[GETARG_A(i)];

#if 0
    mrb_value sclass = mrb_obj_value(mrb_obj_class(mrb, regs[0]));
    printf("%s#%s -> ", 
	   RSTRING_PTR(mrb_funcall(mrb, sclass, "inspect", 0)), 
	   mrb_sym2name(mrb, mrb->c->ci->mid));
    disp_type(mrb, rinfo);
#endif

    mrb->compile_info.force_compile = 0;
    inLocalLabel();

    /* Set return address from callinfo */
    mov(edx, dword [edi + OffsetOf(mrb_context, ci)]);
    mov(eax, dword [edx + OffsetOf(mrb_callinfo, jit_entry)]);
    test(eax, eax);
    push(eax);
    jnz("@f");
    L(".ret_vm");
    pop(eax);
    gen_exit(*status->pc, 1, 0, status);
    L("@@");
    
    if (can_inline) {
      /* Check exception happened? */
      mov(eax, dword [esi + OffsetOf(mrb_state, exc)]);
      test(eax, eax);
      jnz(".ret_vm");

      /* Inline else part of mrbjit_exec_return_fast (but not ensure call) */
      push(edi);

      mov(edi, edx);


      /* Save return value */
      movsd(xmm0, ptr [ecx + GETARG_A(i) * sizeof(mrb_value)]);
      /* Store return value (bottom of stack always return space) */
      movsd(ptr [ecx], xmm0);

      /* Restore Regs */
      mov(ecx, dword [edi + OffsetOf(mrb_callinfo, stackent)]);
      mov(dword [ebx + VMSOffsetOf(regs)], ecx);

      /* Restore c->stack */
      mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
      mov(dword [eax + OffsetOf(mrb_context, stack)], ecx);

      /* pop ci */
      mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
      sub(edi, (Xbyak::uint32)sizeof(mrb_callinfo));
      mov(dword [eax + OffsetOf(mrb_context, ci)], edi);

      /* restore proc */
      mov(edx, dword [edi + OffsetOf(mrb_callinfo, proc)]);
      mov(dword [ebx + VMSOffsetOf(proc)], edx);

      /* restore irep */
      mov(edx, dword [edx + OffsetOf(struct RProc, body.irep)]);
      mov(dword [ebx + VMSOffsetOf(irep)], edx);

      pop(edi);

      ret();
    }
    else {
      /* Update pc */
      mov(dword [ebx + VMSOffsetOf(pc)], (Xbyak::uint32)(*status->pc));

      push(ecx);
      push(ebx);

      lea(eax, dword [ebx + VMSOffsetOf(status)]);
      push(eax);
      push(esi);
      if (can_use_fast) {
	call((void *)mrbjit_exec_return_fast);
      }
      else {
	call((void *)mrbjit_exec_return);
      }
      add(esp, 2 * 4);
      pop(ebx);
      pop(ecx);

      mov(ecx, dword [ebx + VMSOffsetOf(regs)]);

      test(eax, eax);
      jz("@f");
      pop(edx);			/* pop return address from callinfo */
      gen_exit(NULL, 0, 0, status);
      L("@@");

      ret();
    }

    outLocalLabel();

    return code;
  }

  const void *
    emit_return_inline(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_value *regs = *status->regs;

#if 0
    mrb_code *pc = *status->pc;
    mrb_code i = *pc;
    mrbjit_reginfo *rinfo = &coi->reginfo[GETARG_A(i)];
    mrb_value sclass = mrb_obj_value(mrb_obj_class(mrb, regs[0]));
    printf("%s#%s -> ", 
	   RSTRING_PTR(mrb_funcall(mrb, sclass, "inspect", 0)), 
	   mrb_sym2name(mrb, mrb->c->ci->mid));
    disp_type(mrb, rinfo);
#endif

    CALL_CFUNC_BEGIN;
    CALL_CFUNC_STATUS(mrbjit_exec_return, 0);

    mov(ecx, dword [ebx + VMSOffsetOf(regs)]);

    return code;
  }

#define OVERFLOW_CHECK_GEN(AINSTF)                                      \
    jno("@f");                                                          \
    cvtsi2sd(xmm0, dword [ecx + reg0off]);                              \
    cvtsi2sd(xmm1, dword [ecx + reg1off]);				\
    AINSTF(xmm0, xmm1);                                                 \
    movsd(ptr [ecx + reg0off], xmm0);                                   \
    gen_exit(*status->pc + 1, 1, 1, status);				\
    L("@@");                                                            \


#define ARTH_GEN(AINSTI, AINSTF)                                        \
  do {                                                                  \
    int reg0pos = GETARG_A(**ppc);                                      \
    int reg1pos = reg0pos + 1;                                          \
    const Xbyak::uint32 reg0off = reg0pos * sizeof(mrb_value);          \
    const Xbyak::uint32 reg1off = reg1pos * sizeof(mrb_value);          \
    enum mrb_vtype r0type = (enum mrb_vtype) mrb_type(regs[reg0pos]);   \
    enum mrb_vtype r1type = (enum mrb_vtype) mrb_type(regs[reg1pos]);   \
    mrbjit_reginfo *dinfo = &coi->reginfo[reg0pos];                     \
\
    if (r0type == MRB_TT_FIXNUM && r1type == MRB_TT_FIXNUM) {           \
      gen_type_guard(mrb, reg0pos, status, *ppc, coi);			\
      gen_type_guard(mrb, reg1pos, status, *ppc, coi);			\
\
      mov(eax, dword [ecx + reg0off]);                                  \
      AINSTI(eax, dword [ecx + reg1off]);			        \
      OVERFLOW_CHECK_GEN(AINSTF);                                       \
      mov(dword [ecx + reg0off], eax);                                  \
      dinfo->type = MRB_TT_FIXNUM;  					\
      dinfo->klass = mrb->fixnum_class; 				\
    }                                                                   \
    else if ((r0type == MRB_TT_FLOAT || r0type == MRB_TT_FIXNUM) &&     \
             (r1type == MRB_TT_FLOAT || r1type == MRB_TT_FIXNUM)) {	\
      gen_type_guard(mrb, reg0pos, status, *ppc, coi);			\
      gen_type_guard(mrb, reg1pos, status, *ppc, coi);			\
\
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
      dinfo->type = MRB_TT_FLOAT;                                       \
      dinfo->klass = mrb->float_class;                                  \
    }                                                                   \
    else {                                                              \
      emit_send(mrb, status, coi);                                      \
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
    mrbjit_reginfo *dinfo = &coi->reginfo[reg0pos];

    gen_type_guard(mrb, reg0pos, status, *ppc, coi);
    gen_type_guard(mrb, reg1pos, status, *ppc, coi);

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

    /* Div returns Float always */
    /* see http://qiita.com/monamour555/items/bcef9b41a5cc4670675a */
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return code;
  }

#define OVERFLOW_CHECK_I_GEN(AINSTF)                                    \
    jno("@f");                                                          \
    cvtsi2sd(xmm0, dword [ecx + off]);                                  \
    mov(eax, y);                                                        \
    cvtsi2sd(xmm1, eax);                                                \
    AINSTF(xmm0, xmm1);                                                 \
    movsd(ptr [ecx + off], xmm0);                                       \
    gen_exit(*status->pc + 1, 1, 1, status);				\
    L("@@");                                                            \

#define ARTH_I_GEN(AINSTI, AINSTF)                                      \
  do {                                                                  \
    const Xbyak::uint32 y = GETARG_C(**ppc);                            \
    int regno = GETARG_A(**ppc);                                        \
    const Xbyak::uint32 off = regno * sizeof(mrb_value);                \
    enum mrb_vtype atype = (enum mrb_vtype) mrb_type(regs[regno]);      \
    mrbjit_reginfo *dinfo = &coi->reginfo[regno];                       \
\
    gen_type_guard(mrb, regno, status, *ppc, coi);			\
\
    if (atype == MRB_TT_FIXNUM) {                                       \
      mov(eax, dword [ecx + off]);                                      \
      AINSTI(eax, y);                                                   \
      OVERFLOW_CHECK_I_GEN(AINSTF);                                     \
      mov(dword [ecx + off], eax);                                      \
      dinfo->type = MRB_TT_FIXNUM;       				\
      dinfo->klass = mrb->fixnum_class; 				\
    }                                                                   \
    else if (atype == MRB_TT_FLOAT) {					\
      movsd(xmm0, ptr [ecx + off]);                                     \
      mov(eax, y);                                                      \
      cvtsi2sd(xmm1, eax);                                              \
      AINSTF(xmm0, xmm1);                                               \
      movsd(ptr [ecx + off], xmm0);                                     \
      dinfo->type = MRB_TT_FLOAT;					\
      dinfo->klass = mrb->float_class;  				\
    }                                                                   \
    else {                                                              \
      gen_exit(*ppc, 1, 0, status);					\
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
    CMPINST;     						     \
} while(0)

#define COMP_GEN_IF(CMPINST)                                         \
do {                                                                 \
    cvtsi2sd(xmm0, ptr [ecx + off0]);                                \
    xor(eax, eax);					             \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST;    						     \
} while(0)

#define COMP_GEN_FI(CMPINST)                                         \
do {                                                                 \
    movsd(xmm0, ptr [ecx + off0]);				     \
    cvtsi2sd(xmm1, ptr [ecx + off1]);                                \
    xor(eax, eax);					             \
    comisd(xmm0, xmm1);     			                     \
    CMPINST;     						     \
} while(0)

#define COMP_GEN_FF(CMPINST)                                         \
do {                                                                 \
    movsd(xmm0, dword [ecx + off0]);                                 \
    xor(eax, eax);					             \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST;    						     \
} while(0)
    
#define COMP_GEN_SS(CMPINST)                                         \
do {                                                                 \
    push(ecx);                                                       \
    push(ebx);                                                       \
    mov(eax, dword [ecx + off1 + 4]);                                \
    push(eax);                                                       \
    mov(eax, dword [ecx + off1]);                                    \
    push(eax);                                                       \
    mov(eax, dword [ecx + off0 + 4]);                                \
    push(eax);                                                       \
    mov(eax, dword [ecx + off0]);                                    \
    push(eax);                                                       \
    push(esi);                                                       \
    call((void *)mrb_str_cmp);                                       \
    add(esp, sizeof(mrb_state *) + sizeof(mrb_value) * 2);           \
    pop(ebx);                                                        \
    pop(ecx);                                                        \
    test(eax, eax);                                                  \
    CMPINST;    						     \
} while(0)

#define COMP_GEN_CMP(CMPINSTI, CMPINSTF)			     \
do {                                                                 \
    if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&                     \
             mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {           \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_FI(CMPINSTF);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&               \
             mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {            \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_IF(CMPINSTF);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_FLOAT &&                \
             mrb_type(regs[regno + 1]) == MRB_TT_FLOAT) {            \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_FF(CMPINSTF);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_FIXNUM &&               \
             mrb_type(regs[regno + 1]) == MRB_TT_FIXNUM) {           \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_II(CMPINSTI);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_STRING &&               \
             mrb_type(regs[regno + 1]) == MRB_TT_STRING) {           \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_SS(CMPINSTI);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == mrb_type(regs[regno + 1])) {   \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_II(CMPINSTI);                                     \
    }                                                                \
    else if (mrb_type(regs[regno]) == MRB_TT_FALSE ||                \
             mrb_type(regs[regno + 1]) == MRB_TT_FALSE) {            \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_II(CMPINSTI);                                     \
    }                                                                \
    else {                                                           \
        /* never reach  */                                           \
        assert(0);                                                   \
    }                                                                \
 } while(0)

#define COMP_BOOL_SET                                                \
do {								     \
    xor(ah, ah);                                                     \
    cwde();                                                          \
    add(eax, eax);                                                   \
    or(eax, 0xfff00001);                                             \
    mov(dword [ecx + off0 + 4], eax);                                \
  } while(0)

#define COMP_GEN(CMPINSTI, CMPINSTF)			             \
do {                                                                 \
    int regno = GETARG_A(**ppc);                                     \
    const Xbyak::uint32 off0 = regno * sizeof(mrb_value);            \
    const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);             \
                                                                     \
    COMP_GEN_CMP(CMPINSTI, CMPINSTF);                                \
    COMP_BOOL_SET;                                                   \
 } while(0)

#define COMP_AND_JMP(CMPINSTI, CMPINSTF)                 	     \
do {                                                                 \
    int regno = GETARG_A(**ppc);                                     \
    const Xbyak::uint32 off0 = regno * sizeof(mrb_value);            \
    const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);             \
                                                                     \
    COMP_GEN_CMP(CMPINSTI, CMPINSTF);                                \
 } while(0)

#define COMP_JMPNOT(CMPINSTI, CMPINSTF, NCMPINSTI, NCMPINSTF)	     \
do {                                                                 \
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);		     \
  if (!b) {                                                          \
    COMP_AND_JMP(NCMPINSTI, NCMPINSTF);                              \
    gen_exit(*ppc + 3, 1, 0, status);                                \
    dinfo->type = MRB_TT_FALSE;                                      \
    dinfo->klass = mrb->false_class;                                 \
  }                                                                  \
  else {                                                             \
    COMP_AND_JMP(CMPINSTI, CMPINSTF);                                \
    gen_exit(*ppc + 2 + GETARG_sBx(jmpc), 1, 0, status);             \
    dinfo->type = MRB_TT_TRUE;                                       \
    dinfo->klass = mrb->true_class;                                  \
  }                                                                  \
  L("@@");                                                           \
  dinfo->constp = 1;                                                 \
} while(0)

#define COMP_JMPIF(CMPINSTI, CMPINSTF, NCMPINSTI, NCMPINSTF)	     \
do {                                                                 \
  const Xbyak::uint32 off0 = regno * sizeof(mrb_value);		     \
  if (b) {                                                           \
    COMP_AND_JMP(CMPINSTI, CMPINSTF);                                \
    gen_exit(*ppc + 3, 1, 0, status);                                \
    dinfo->type = MRB_TT_TRUE;                                       \
    dinfo->klass = mrb->false_class;                                 \
  }                                                                  \
  else {                                                             \
    COMP_AND_JMP(NCMPINSTI, NCMPINSTF);                              \
    gen_exit(*ppc + 2 + GETARG_sBx(jmpc), 1, 0, status);             \
    dinfo->type = MRB_TT_FALSE;                                      \
    dinfo->klass = mrb->false_class;                                 \
  }                                                                  \
  L("@@");                                                           \
  dinfo->constp = 1;                                                 \
} while(0)

  const void *
    emit_eq(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int regno = GETARG_A(**ppc);
    enum mrb_vtype tt = (enum mrb_vtype) mrb_type(regs[regno]);
    mrbjit_reginfo *dinfo = &coi->reginfo[regno];

    //    int b;
    //mrb_code jmpc = *(*ppc + 2);

    /* Import from class.h */
    switch (tt) {
    case MRB_TT_TRUE:
    case MRB_TT_FALSE:
    case MRB_TT_SYMBOL:
    case MRB_TT_FIXNUM:
    case MRB_TT_FLOAT:
    case MRB_TT_STRING:
#if 0
      mrb->compile_info.disable_jit = 1;
      b = mrb_test(mrb_funcall(mrb, regs[regno], "==", 1, regs[regno + 1]));
      mrb->compile_info.disable_jit = 0;
      switch (GET_OPCODE(jmpc)) {
      case OP_JMPNOT:
	COMP_JMPNOT(jz("@f"), jz("@f"), jnz("@f"), jnz("@f"));
	return code;

      case OP_JMPIF:
	COMP_JMPIF(jz("@f"), jz("@f"), jnz("@f"), jnz("@f"));
	return code;

      default:
	break;
      }
#endif
      
      COMP_GEN(setz(al), setz(al));
      break;

    default:
      gen_exit(*status->pc, 1, 1, status);
      break;
    }

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    return code;
  }

  const void *
    emit_lt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

#if 0
    int b;
    mrb_code jmpc = *(*ppc + 2);
    int regno = GETARG_A(**ppc);

    mrb->compile_info.disable_jit = 1;
    b = mrb_test(mrb_funcall(mrb, regs[regno], "<", 1, regs[regno + 1]));
    mrb->compile_info.disable_jit = 0;
    switch (GET_OPCODE(jmpc)) {
    case OP_JMPNOT:
      COMP_JMPNOT(jl("@f"), jb("@f"), jge("@f"), jae("@f"));
      return code;

    case OP_JMPIF:
      COMP_JMPIF(jl("@f"), jb("@f"), jge("@f"), jae("@f"));
      return code;

    default:
      break;
    }
#endif

    COMP_GEN(setl(al), setb(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    return code;
  }

  const void *
    emit_le(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    COMP_GEN(setle(al), setbe(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    return code;
  }

  const void *
    emit_gt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    COMP_GEN(setg(al), seta(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    return code;
  }

  const void *
    emit_ge(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    COMP_GEN(setge(al), setae(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    return code;
  }

  const void *
    emit_array(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    int siz = GETARG_C(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    mov(eax, ptr [esi + OffsetOf(mrb_state, arena_idx)]);
    push(eax);
    push(ecx);
    push(ebx);

    lea(eax, ptr [ecx + srcoff]);
    push(eax);
    mov(eax, siz);
    push(eax);
    push(esi);
    call((void *) mrb_ary_new_from_values);
    add(esp, sizeof(mrb_state *) + sizeof(int) + sizeof(mrb_value *));
    
    pop(ebx);
    pop(ecx);

    mov(ptr [ecx + dstoff], eax);
    mov(ptr [ecx + dstoff + 4], edx);

    pop(eax);
    mov(ptr [esi + OffsetOf(mrb_state, arena_idx)], eax);

    dinfo->type = MRB_TT_ARRAY;
    dinfo->klass = mrb->array_class;
    dinfo->constp = 0;
    return code;
  }

  const void *
    emit_arycat(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    mov(eax, ptr [esi + OffsetOf(mrb_state, arena_idx)]);
    push(eax);

    push(ecx);
    push(ebx);

    mov(eax, ptr [ecx + srcoff + 4]);
    push(eax);
    mov(eax, ptr [ecx + srcoff]);
    push(eax);
    push(esi);
    call((void *) mrb_ary_splat);
    add(esp, sizeof(mrb_state *) + sizeof(mrb_value));

    pop(ebx);
    pop(ecx);

    push(ecx);
    push(ebx);

    /* rc of splat */
    push(edx);
    push(eax);
    /* arg1 reg */
    mov(eax, ptr [ecx + dstoff + 4]);
    push(eax);
    mov(eax, ptr [ecx + dstoff]);
    push(eax);
    /* mrb */
    push(esi);
    call((void *) mrb_ary_concat);
    add(esp, sizeof(mrb_state *) + sizeof(mrb_value) * 2);
    
    pop(ebx);
    pop(ecx);

    mov(ptr [ecx + dstoff], eax);
    mov(ptr [ecx + dstoff + 4], edx);

    pop(eax);
    mov(ptr [esi + OffsetOf(mrb_state, arena_idx)], eax);

    dinfo->type = MRB_TT_ARRAY;
    dinfo->klass = mrb->array_class;
    dinfo->constp = 0;
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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    Xbyak::uint32 i;
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;

    mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);
    mov(eax, dword [eax + OffsetOf(mrb_callinfo, proc)]);
    mov(eax, dword [eax + OffsetOf(struct RProc, env)]);
    for (i = 0; i < uppos; i++) {
      mov(eax, dword [eax + OffsetOf(struct REnv, c)]);
    }
    mov(eax, dword [eax + OffsetOf(struct REnv, stack)]);

    movsd(xmm0, ptr [eax + idxpos * sizeof(mrb_value)]);
    movsd(ptr [ecx + dstoff], xmm0);

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
    Xbyak::uint32 i;

    mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);
    mov(eax, dword [eax + OffsetOf(mrb_callinfo, proc)]);
    mov(eax, dword [eax + OffsetOf(struct RProc, env)]);
    for (i = 0; i < uppos; i++) {
      mov(eax, dword [eax + OffsetOf(struct REnv, c)]);
    }
    mov(eax, dword [eax + OffsetOf(struct REnv, stack)]);

    movsd(xmm0, ptr [ecx + valoff]);
    movsd(ptr [eax + idxpos * sizeof(mrb_value)], xmm0);

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
    mrbjit_reginfo *rinfo = &coi->reginfo[cond];
    
    mov(eax, ptr [ecx + coff + 4]);
    if (mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 1, *ppc + 1, status, rinfo);
      gen_jmp(mrb, status, *ppc, *ppc + GETARG_sBx(**ppc));
    }
    else {
      gen_bool_guard(mrb, 0, *ppc + GETARG_sBx(**ppc), status, rinfo);
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
    mrbjit_reginfo *rinfo = &coi->reginfo[cond];

    mov(eax, ptr [ecx + coff + 4]);
    if (!mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 0, *ppc + 1, status, rinfo);
      gen_jmp(mrb, status, *ppc, *ppc + GETARG_sBx(**ppc));
    }
    else {
      gen_bool_guard(mrb, 1, *ppc + GETARG_sBx(**ppc), status, rinfo);
    }

    return code;
  }

  const void *
    emit_lambda(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int i;
    const int lno = GETARG_b(**ppc);
    const int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int flags = GETARG_C(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_irep *mirep =irep->reps[lno];
    struct mrb_context *c = mrb->c;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_PROC;
    dinfo->klass = mrb->proc_class;
    dinfo->constp = 1;

    if (mirep->shared_lambda && c->proc_pool && 0) {
      for (i = -1; c->proc_pool[i].proc.tt == MRB_TT_PROC; i--) {
	if (c->proc_pool[i].proc.body.irep == mirep) {
	  struct RProc *nproc = &c->proc_pool[i].proc;
	  mov(dword [ecx + dstoff], (Xbyak::uint32)nproc);
	  mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_PROC);
	  /* mov(edx, (Xbyak::uint32)nproc->env);
	     mov(dword [edx + OffsetOf(struct REnv, stack)], ecx);
	     mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
	     mov(eax, dword [eax + OffsetOf(mrb_context, ci)]);
	     mov(eax, dword [eax + OffsetOf(mrb_callinfo, proc)]);
	     mov(eax, dword [eax + OffsetOf(struct RProc, env)]);
	     mov(dword [edx + OffsetOf(struct REnv, c)], eax); */
	  /*
                  push(ecx);
                  push(ebx);
                  mov(eax, dword [ecx + dstoff + 4]);
		  push(eax);
		  mov(eax, dword [ecx + dstoff]);
		  push(eax);
		  push(esi);
		  call((void *)mrb_p);
		  add(esp, 12);
		  pop(ebx);
		  pop(ecx);
	  */
	  return code;
	}
      }
    }

    mov(eax, ptr [esi + OffsetOf(mrb_state, arena_idx)]);
    push(eax);
    push(ecx);
    push(ebx);
    mov(eax, (Xbyak::uint32)mirep);
    push(eax);
    push(esi);
    if (flags & OP_L_CAPTURE) {
      call((void *) mrb_closure_new);
    }
    else {
      call((void *) mrb_proc_new);
    }
    add(esp, 2 * sizeof(void *));
    pop(ebx);
    pop(ecx);
    mov(dword [ecx + dstoff], eax);
    mov(dword [ecx + dstoff + 4], 0xfff00000 | MRB_TT_PROC);
    if (flags & OP_L_STRICT) {
      mov(edx, (Xbyak::uint32)MRB_PROC_STRICT);
      shl(edx, 11);
      or(ptr [eax], edx);
    }
    pop(eax);
    mov(ptr [esi + OffsetOf(mrb_state, arena_idx)], eax);
    return code;
  }

  const void *
    emit_range(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff0 = GETARG_B(**ppc) * sizeof(mrb_value);
    int srcoff1 = srcoff0 + sizeof(mrb_value);
    int exelp = GETARG_C(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_RANGE;
    dinfo->klass = mrb_class(mrb, 
			     mrb_vm_const_get(mrb, mrb_intern_cstr(mrb, "Range")));

    push(ecx);
    push(ebx);

    mov(eax, exelp);
    push(eax);
    mov(eax, ptr [ecx + srcoff1 + 4]);
    push(eax);
    mov(eax, ptr [ecx + srcoff1]);
    push(eax);
    mov(eax, ptr [ecx + srcoff0 + 4]);
    push(eax);
    mov(eax, ptr [ecx + srcoff0]);
    push(eax);
    push(esi);
    call((void *) mrb_range_new);
    add(esp, sizeof(mrb_state *) + sizeof(mrb_value) * 2 + sizeof(int));
    
    pop(ebx);
    pop(ecx);

    mov(ptr [ecx + dstoff], eax);
    mov(ptr [ecx + dstoff + 4], edx);
    return code;
  }

  const void *
    emit_string(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrb_irep *irep = *status->irep;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrb_value *str = irep->pool + GETARG_Bx(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_RANGE;
    dinfo->klass = mrb_class(mrb, 
			     mrb_vm_const_get(mrb, mrb_intern_cstr(mrb, "String")));

    push(ecx);
    push(ebx);

    mov(eax, (Xbyak::uint32)str);
    mov(edx, dword [eax + 4]);
    push(edx);
    mov(edx, dword [eax]);
    push(edx);
    push(esi);
    call((void *) mrb_str_dup);
    add(esp, sizeof(mrb_state *) + sizeof(mrb_value));
    
    pop(ebx);
    pop(ecx);

    mov(ptr [ecx + dstoff], eax);
    mov(ptr [ecx + dstoff + 4], edx);
    return code;
  }

  const void *
    emit_hash(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    int num = GETARG_C(**ppc);
    int i;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_RANGE;
    dinfo->klass = mrb_class(mrb, 
			     mrb_vm_const_get(mrb, mrb_intern_cstr(mrb, "Hash")));

    push(ecx);
    push(ebx);

    push((Xbyak::uint32)num);
    push(esi);
    call((void *) mrb_hash_new_capa);
    add(esp, sizeof(mrb_state *) + sizeof(int));
    pop(ebx);
    pop(ecx);

    mov(ptr [ecx + dstoff], eax);
    mov(ptr [ecx + dstoff + 4], edx);

    for(i = 0; i < num; i+= 2) {
      push(ecx);
      push(ebx);
      push(edx);
      push(eax);

      /* key */
      mov(ebx, dword [ecx + (srcoff + (i + 1) * sizeof(mrb_value) + 4)]);
      push(ebx);
      mov(ebx, dword [ecx + (srcoff + (i + 1) * sizeof(mrb_value))]);
      push(ebx);

      /* val */
      mov(ebx, dword [ecx + (srcoff + i * sizeof(mrb_value) + 4)]);
      push(ebx);
      mov(ebx, dword [ecx + (srcoff + i * sizeof(mrb_value))]);
      push(ebx);

      /* hash */
      push(edx);
      push(eax);

      /* mrb */
      push(esi);

      call((void *)mrb_hash_set);
      add(esp, sizeof(mrb_state *) + sizeof(mrb_value) * 3);

      pop(eax);
      pop(eax);
      pop(ecx);
      pop(ebx);
    }

    return code;
  }

  /* primitive methodes */
  mrb_value 
    mrbjit_prim_num_cmp_impl(mrb_state *mrb, mrb_value proc,
			     mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_fix_succ_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_fix_mod_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_obj_not_equal_m_impl(mrb_state *mrb, mrb_value proc,
				     mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_ary_aget_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_ary_aset_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_fix_to_f_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_instance_new_impl(mrb_state *mrb, mrb_value proc,
				  mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value
    mrbjit_prim_fiber_resume_impl(mrb_state *mrb, mrb_value proc,
			     mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_enum_all_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_kernel_equal_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_math_sqrt_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);
};

#endif  /* MRUBY_JITCODE_H */

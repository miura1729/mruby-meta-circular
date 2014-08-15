/*
** mruby/jitcode.h - Class for XBYAK
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JITCOD_H
#define MRUBY_JITCODE_H

#include "jit_x86.h"

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
void disasm_irep(mrb_state *, mrb_irep *);
} /* extern "C" */

#define OffsetOf(s_type, field) ((size_t) &((s_type *)0)->field) 
#define VMSOffsetOf(field) (((intptr_t)status->field) - ((intptr_t)status->pc))

class MRBJitCode: public MRBGenericCodeGenerator {

  void *addr_call_extend_callinfo;
  void *addr_call_stack_extend;

 public:

 MRBJitCode()
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
    gen_flush_one(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, int pos)
  {
    mrb_irep *irep = *status->irep;

    switch(coi->reginfo[pos].regplace) {
    case MRBJIT_REG_MEMORY:
      break;

    case MRBJIT_REG_AL:
      /*      printf("%x %x \n", *status->pc, *status->irep);
      disasm_irep(mrb, *status->irep);
      disasm_once(mrb, *status->irep, **status->pc);*/
    //printf("\n\n");
      xor(ah, ah);
      cwde();
      add(eax, eax);
      or(eax, 0xfff00001);
      emit_local_var_type_write(pos * sizeof(mrb_value), reg_tmp0);
      break;
       
    default:
      printf("%d %d %d\n", irep->nregs, coi->reginfo[pos].regplace, pos);
      assert(0);
      break;
    }
  }
  void
    gen_flush_regs(mrb_state *mrb, mrb_code *pc, mrbjit_vmstatus *status, mrbjit_code_info *coi, int updateinfo)
  {
    mrb_irep *irep = *status->irep;
    int i;

    if (!coi || !coi->reginfo) {
      return;
    }

    for (i = 0; i < irep->nregs; i++) {
      gen_flush_one(mrb, status, coi, i);
      if (updateinfo) {
	coi->reginfo[i].regplace = MRBJIT_REG_MEMORY;
	coi->reginfo[i].unboxedp = 0;
      }
    }
  }

  void
    gen_restore_one(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, int pos)
  {
    mrb_irep *irep = *status->irep;

    if (coi->reginfo == NULL) {
      return;
    }

    switch(coi->reginfo[pos].regplace) {
    case MRBJIT_REG_MEMORY:
      break;

    case MRBJIT_REG_AL:
      /*printf("%x %x \n", *status->pc, *status->irep);
      disasm_irep(mrb, *status->irep);
      disasm_once(mrb, *status->irep, **status->pc);*/
    //printf("\n\n");
      emit_local_var_type_read(reg_tmp0, pos * sizeof(mrb_value));
      cmp(eax, 0xfff00001);
      setnz(al);
      break;
       
    default:
      printf("%d %d %d\n", irep->nregs, coi->reginfo[pos].regplace, pos);
      assert(0);
      break;
    }
  }

  void
    gen_restore_regs(mrb_state *mrb, mrb_code *pc, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_irep *irep = *status->irep;
    int i;

    if (!coi || !coi->reginfo) {
      return;
    }

    for (i = 0; i < irep->nregs; i++) {
      gen_restore_one(mrb, status, coi, i);
    }
  }

  void 
    gen_exit(mrb_state *mrb, mrb_code *pc, int is_clr_rc, int is_clr_exitpos, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    inLocalLabel();

    gen_flush_regs(mrb, pc, status, coi, 0);
    L(".exitlab");

    if (pc) {
      mov(dword [ebx + VMSOffsetOf(pc)], (Xbyak::uint32)pc);
    }
    if (is_clr_rc) {
      emit_load_literal(reg_tmp0, 0);
    }
    if (is_clr_exitpos) {
      emit_load_literal(reg_tmp1, 0);
    }
    else {
      //mov(edx, (Xbyak::uint32)exit_ptr);
      mov(edx, ".exitlab");
    }
    ret();
    outLocalLabel();
  }
  
  const void *
    gen_jump_block(mrb_state *mrb, void *entry, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrbjit_code_info *prevcoi)
  {
    const void *code = getCurr();
    mrb_irep *irep = *status->irep;
    int i;

    if (coi && prevcoi && coi->reginfo && prevcoi->reginfo) {
      for (i = 0; i < irep->nregs; i++) {
	if (coi->reginfo[i].regplace != prevcoi->reginfo[i].regplace) {
	  gen_flush_one(mrb, status, prevcoi, i);
	  gen_restore_one(mrb, status, coi, i);
	}
      }
    } else {
      gen_flush_regs(mrb, *status->pc, status, prevcoi, 1);
      gen_restore_regs(mrb, *status->pc, status, coi);
    }
    jmp(entry);

    return code;
  }

  void 
    gen_jmp_patch(void *dst, void *target, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    jmp(target);
    setSize(cursize);
  }

  void 
    gen_exit_patch(mrb_state *mrb, void *dst, mrb_code *pc, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    gen_exit(mrb, pc, 1, 0, status, coi);
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
    gen_jmp(mrb_state *mrb, mrbjit_vmstatus *status, mrb_code *curpc, mrb_code *newpc, mrbjit_code_info *coi)
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
#if 0
      mrb_irep *irep = *status->irep;
      int i;

      printf("Irep %x Pc %x \n", irep, *status->pc);
      for (i = 0; i < irep->nregs; i++) {
	printf("%d -> %d, %d\n", i, newci->reginfo[i].regplace, coi->reginfo[i].regplace);
	printf("  -> %d, %d\n", newci->reginfo[i].type, coi->reginfo[i].type);
      }
      printf("\n");
#endif

      if (newci->used > 0) {
	jmp((void *)newci->entry);
	mrb->compile_info.code_base = NULL;
      }
      /*else {
	newci->entry = (void *(*)())getCurr();
	gen_exit(newpc, 1, 0, status, coi);
	}*/
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

    /* Get type tag */
    emit_local_var_type_read(reg_tmp0, regpos * sizeof(mrb_value));
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
    gen_exit(mrb, pc, 1, 0, status, coi);

    L("@@");
  }

  /*
   input EAX Pointer to tested boolean
  */
  void
    gen_bool_guard(mrb_state *mrb, int b, int cond, mrb_code *pc, 
		   mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrbjit_reginfo *rinfo = &coi->reginfo[cond];

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
    gen_exit(mrb, pc, 1, 0, status, coi);

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

      emit_local_var_type_read(reg_tmp0, regpos * sizeof(mrb_value));

      if (tt == MRB_TT_FLOAT) {
	cmp(eax, 0xfff00000);
	jb("@f");
      }
      else {
	cmp(eax, 0xfff00000 | tt);
	jz("@f");
      }

      /* Guard fail exit code */
      gen_exit(mrb, pc, 1, 0, status, coi);

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
	emit_local_var_value_read(reg_tmp0, regpos * sizeof(mrb_value));
	mov(eax, dword [eax + OffsetOf(struct RBasic, c)]);
	cmp(eax, (int)c);
	jz("@f");
	/* Guard fail exit code */
	gen_exit(mrb, pc, 1, 0, status, coi);

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
    emit_load_literal(reg_tmp1, (Xbyak::uint32)ctab);
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

#ifdef ENABLE_DEBUG
  void
    gen_call_fetch_hook(mrb_state *mrb, mrbjit_vmstatus *status)
  {
    push(edx);
    push(eax);
    emit_cfunc_start();
    push(ecx);
    //    mov(eax, dword [ebx + VMSOffsetOf(pc)]);
    emit_load_literal(reg_tmp0, (Xbyak::uint32)(*(status->pc)));
    push(eax);
    emit_load_literal(reg_tmp0, (Xbyak::uint32)(*(status->irep)));
    push(eax);
    push(esi);
    call((void *)mrb->code_fetch_hook);
    emit_cfunc_end(sizeof(void *) * 4);
    pop(eax);
    pop(edx);
  }
#endif

  void 
    gen_send_mruby(mrb_state *mrb, struct RProc *m, mrb_sym mid, mrb_value recv, 
		   struct RClass *c, mrbjit_vmstatus *status, mrb_code *pc, 
		   mrbjit_code_info *coi)
  {
    int callee_nregs;
    mrb_irep *irep = *status->irep;
    int i = *pc;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
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
      push(ecx);
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
      emit_cfunc_end(3 * sizeof(void *));
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

    emit_load_literal(reg_tmp0, 0);
    mov(dword [edi + OffsetOf(mrb_callinfo, env)], eax);
    mov(dword [edi + OffsetOf(mrb_callinfo, jit_entry)], eax);
    mov(dword [edi + OffsetOf(mrb_callinfo, err)], eax);

    if (n == CALL_MAXARGS) {
      emit_load_literal(reg_tmp0, -1);
    }
    else {
      emit_load_literal(reg_tmp0, n);
    }
    mov(dword [edi + OffsetOf(mrb_callinfo, argc)], reg_tmp0);

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

    emit_load_literal(reg_tmp0, (Xbyak::uint32)mid);
    mov(dword [edi + OffsetOf(mrb_callinfo, mid)], eax);

    emit_load_literal(reg_tmp0, (Xbyak::uint32)a);
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
	emit_load_literal(reg_tmp1, 
			  (Xbyak::uint32)((callee_nregs < 3) ? 3 : 
					  callee_nregs));
	emit_load_literal(reg_tmp0, 3);
      }
      else {
	emit_load_literal(reg_tmp1, (Xbyak::uint32)callee_nregs);
	emit_load_literal(reg_tmp0, (Xbyak::uint32)(mrb->c->ci->argc + 2));
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
	emit_load_literal(reg_tmp1, (Xbyak::uint32)((callee_nregs < 3) ? 3 : 
						    callee_nregs));
	emit_load_literal(reg_tmp0, 3);
      }
      else {
	emit_load_literal(reg_tmp1, (Xbyak::uint32)callee_nregs);
	emit_load_literal(reg_tmp0, (Xbyak::uint32)(mrb->c->ci->argc + 2));
      }
      call(addr_call_stack_extend);
    }
      
    L("@@");

    mov(dword [ebx + VMSOffsetOf(regs)], ecx);

    gen_set_jit_entry(mrb, pc, coi, irep);

    if (is_block_call) {
      mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);
      emit_local_var_value_read(reg_tmp1, 0); /* self */
      mov(dword [eax + OffsetOf(mrb_callinfo, proc)], edx);
      mov(dword [ebx + VMSOffsetOf(proc)], edx);

      mov(edx, dword [edx + OffsetOf(struct RProc, target_class)]);
      mov(dword [eax + OffsetOf(mrb_callinfo, target_class)], edx);

      emit_local_var_value_read(reg_tmp1, 0); /* self */
      mov(edx, dword [edx + OffsetOf(struct RProc, body.irep)]);
      mov(dword [ebx + VMSOffsetOf(irep)], edx);

      mov(edx, dword [edx + OffsetOf(mrb_irep, nregs)]);
      mov(dword [eax + OffsetOf(mrb_callinfo, nregs)], edx);

      emit_local_var_value_read(reg_tmp1, 0); /* self */
      mov(edx, dword [edx + OffsetOf(struct RProc, env)]);
      mov(edx, dword [edx + OffsetOf(struct REnv, mid)]);
      test(edx, edx);
      jz("@f");

      mov(dword [eax + OffsetOf(mrb_callinfo, mid)], edx);
      
      L("@@");

      emit_local_var_value_read(reg_tmp0, 0); /* self */
      mov(edx, dword [eax + OffsetOf(struct RProc, body.irep)]);
      mov(edx, dword [edx + OffsetOf(mrb_irep, iseq)]);
      mov(dword [ebx + VMSOffsetOf(pc)], edx);

      mov(edx, dword [eax + OffsetOf(struct RProc, env)]);
      mov(edx, dword [edx + OffsetOf(struct REnv, stack)]);
      movsd(xmm0, ptr [edx]);
      mov(ecx, dword [edi + OffsetOf(mrb_context, stack)]);
      movsd(ptr [ecx], xmm0);

      mrb->compile_info.force_compile = 1;
    }
  }

  const void *
    ent_nop(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    return code;
  }

  const void *
    ent_move(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *sinfo = &coi->reginfo[GETARG_B(**ppc)];
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (coi && coi->reginfo) {
      gen_flush_one(mrb, status, coi, GETARG_B(**ppc));
    }
    sinfo->regplace = MRBJIT_REG_MEMORY;
    sinfo->unboxedp = 0;
    *dinfo = *sinfo;

    emit_local_var_read(reg_dtmp0, srcoff);
    emit_local_var_write(dstoff, reg_dtmp0);
    return code;
  }

  const void *
    ent_loadl(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_load_literal(reg_tmp0, (Xbyak::uint32)irep->pool + srcoff);
    movsd(xmm0, ptr [eax]);
    emit_local_var_write(dstoff, reg_dtmp0);

    return code;
  }

  const void *
    ent_loadi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const Xbyak::uint32 src = GETARG_sBx(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    emit_load_literal(reg_tmp0, src);
    emit_local_var_value_write(dstoff, reg_tmp0);
    if (dinfo->type != MRB_TT_FIXNUM) {
      emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_FIXNUM);
      emit_local_var_type_write(dstoff, reg_tmp0);
      dinfo->type = MRB_TT_FIXNUM;
      dinfo->klass = mrb->fixnum_class;
    }
    dinfo->constp = 1;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    return code;
  }

  const void *
    ent_loadsym(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_load_literal(reg_tmp0, src);
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_SYMBOL);
    emit_local_var_type_write(dstoff, reg_tmp0);

    return code;
  }

  const void *
    ent_loadself(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int a = GETARG_A(**ppc);
    const Xbyak::uint32 dstoff = a * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[a];
    mrb_value self = *status->regs[0];

    dinfo->type = (mrb_vtype)mrb_type(self);
    dinfo->klass = mrb->c->ci->target_class;
    dinfo->constp = 1;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_local_var_read(reg_dtmp0, 0); /* self */
    emit_local_var_write(dstoff, reg_dtmp0);
    return code;
  }

  const void *
    ent_loadt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (dinfo->type != MRB_TT_TRUE) {
      emit_load_literal(reg_tmp0, 1);
      emit_local_var_value_write(dstoff, reg_tmp0);
      emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_TRUE);
      emit_local_var_type_write(dstoff, reg_tmp0);
      dinfo->type = MRB_TT_TRUE;
      dinfo->klass = mrb->true_class;
      dinfo->constp = 1;
    }

    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_loadf(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (dinfo->type != MRB_TT_FALSE) {
      emit_load_literal(reg_tmp0, 1);
      emit_local_var_value_write(dstoff, reg_tmp0);
      emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_FALSE);
      emit_local_var_type_write(dstoff, reg_tmp0);
      dinfo->type = MRB_TT_FALSE;
      dinfo->klass = mrb->false_class;
      dinfo->constp = 1;
    }

    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_getglobal(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_cfunc_start();
    push((Xbyak::uint32)irep->syms[idpos]);
    push(esi);
    call((void *)mrb_gv_get);
    emit_cfunc_end(argsize);
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);

    return code;
  }

  const void *
    ent_setglobal(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    const Xbyak::uint32 srcoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 4 * sizeof(void *);
    mrb_irep *irep = *status->irep;

    emit_cfunc_start();
    emit_local_var_type_read(reg_tmp0, srcoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff);
    push(eax);
    push((Xbyak::uint32)irep->syms[idpos]);
    push(esi);
    call((void *)mrb_gv_set);
    emit_cfunc_end(argsize);

    return code;
  }

  const void *
    ent_getiv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    if (ivoff < 0) {
      return NULL;
    }

    /* You can not change class of self in Ruby */
    if (mrb_type(self) == MRB_TT_OBJECT) {
      emit_local_var_value_read(reg_tmp0, 0); /* self */
      mov(eax, dword [eax + OffsetOf(struct RObject, ivent.rootseg)]);
      movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);
    }
    else {
      emit_local_var_value_read(reg_tmp0, 0); /* self */
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      mov(eax, dword [eax]);
      movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);
    }
    emit_local_var_write(dstoff, reg_dtmp0);

    return code;
  }

  const void *
    ent_setiv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 srcoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int idpos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_sym id = irep->syms[idpos];
    mrb_value self = mrb->c->stack[0];
    int ivoff = mrbjit_iv_off(mrb, self, id);

    inLocalLabel();

    if (ivoff != -1) {
      emit_local_var_value_read(reg_tmp0, 0); /* self */
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      test(eax, eax);
      jz(".nivset");
      mov(eax, dword [eax]);
      test(eax, eax);
      jnz(".fastivset");
    }

    L(".nivset");
    /* Normal instance variable set (not define iv yet) */
    emit_cfunc_start();
    emit_local_var_type_read(reg_tmp0, srcoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff);
    push(eax);
    push((Xbyak::uint32)id);
    push(esi);
    call((void *)mrb_vm_iv_set);
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(Xbyak::uint32) + sizeof(mrb_value));

    if (ivoff != -1) {
      jmp(".ivsetend");

      L(".fastivset");
      emit_local_var_read(reg_dtmp0, srcoff);
      emit_local_var_value_read(reg_tmp0, 0); /* self */
      push(eax);
      emit_cfunc_start();
      push(eax);
      push(esi);
      call((void *)mrb_write_barrier);
      emit_cfunc_end(sizeof(mrb_state *) + sizeof(struct RBasic *));
      pop(eax);
      if (ivoff == -2) {
	if (mrb_type(self) == MRB_TT_OBJECT) {
	  mov(edx, eax);
	}
	mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
	ivoff =  mrb_obj_ptr(self)->iv->last_len;
	inc(dword [eax + OffsetOf(iv_tbl, last_len)]);
	mov(eax, dword [eax]);
	movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);
	mov(dword [eax + MRB_SEGMENT_SIZE * sizeof(mrb_value) + ivoff * sizeof(mrb_sym)], (Xbyak::uint32)id);
      }
      else {
	mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
	mov(eax, dword [eax]);
	movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);
      }

      L(".ivsetend");
    }
    outLocalLabel();
    return code;
  }

  const void *
    ent_getcv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_cfunc_start();
    push((Xbyak::uint32)irep->syms[idpos]);
    push(esi);
    call((void *)mrb_vm_cv_get);
    emit_cfunc_end(argsize);
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);

    return code;
  }

  const void *
    ent_setcv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    const Xbyak::uint32 srcoff = GETARG_A(**ppc) * sizeof(mrb_value);
    const int argsize = 4 * sizeof(void *);
    mrb_irep *irep = *status->irep;

    emit_cfunc_start();
    emit_local_var_type_read(reg_tmp0, srcoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff);
    push(eax);
    push((Xbyak::uint32)irep->syms[idpos]);
    push(esi);
    call((void *)mrb_vm_cv_set);
    emit_cfunc_end(argsize);

    return code;
  }

  const void *
    ent_getconst(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_load_literal(reg_tmp0, v.value.i);
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_load_literal(reg_tmp0, v.value.ttt);
    emit_local_var_type_write(dstoff, reg_tmp0);
    
    return code;
  }

  const void *
    ent_getmconst(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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

    emit_load_literal(reg_tmp0, v.value.i);
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_load_literal(reg_tmp0, v.value.ttt);
    emit_local_var_type_write(dstoff, reg_tmp0);
    
    return code;
  }

  const void *
    ent_loadnil(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const Xbyak::uint32 dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FALSE;
    dinfo->klass = mrb->nil_class;
    dinfo->constp = 1;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_load_literal(reg_tmp0, 0);
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_FALSE);
    emit_local_var_type_write(dstoff, reg_tmp0);

    return code;
  }

#define CALL_CFUNC_BEGIN                                             \
  do {                                                               \
    emit_cfunc_start();						     \
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
    emit_cfunc_end((auxargs + 2) * 4);				     \
\
    test(eax, eax);					             \
    jz("@f");                                                        \
    gen_exit(mrb, NULL, 0, 1, status, coi);			     \
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
    ent_send(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(i)];
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;
    
    if (GETARG_C(i) == CALL_MAXARGS) {
      n = 1;
    }

    recv = regs[a];
    c = mrb_class(mrb, recv);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (!m) {
      return NULL;
    }

    gen_flush_regs(mrb, pc, status, coi, 1);
    gen_class_guard(mrb, a, status, pc, coi, mrb_class(mrb, recv));

    if ((ivid = is_reader(mrb, m))) {
      const int ivoff = mrbjit_iv_off(mrb, recv, ivid);

      /* Inline IV reader */
      emit_local_var_value_read(reg_tmp0, a * sizeof(mrb_value));
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      mov(eax, dword [eax]);
      movsd(xmm0, ptr [eax + ivoff * sizeof(mrb_value)]);

      // regs[a] = obj;
      emit_local_var_write(a * sizeof(mrb_value), reg_dtmp0);

      return code;
    }

    if ((ivid = is_writer(mrb, m))) {
      const int ivoff = mrbjit_iv_off(mrb, recv, ivid);

      /* Inline IV writer */
      emit_local_var_value_read(reg_tmp0, a * sizeof(mrb_value));
      mov(eax, dword [eax + OffsetOf(struct RObject, iv)]);
      mov(eax, dword [eax]);

      // @iv = regs[a];
      emit_local_var_read(reg_dtmp0, (a + 1) * sizeof(mrb_value));
      movsd(ptr [eax + ivoff * sizeof(mrb_value)], xmm0);

      return code;
    }

    if (GET_OPCODE(i) != OP_SENDB) {
      //SET_NIL_VALUE(regs[a+n+1]);
      int dstoff = (a + n + 1) * sizeof(mrb_value);
      emit_load_literal(reg_tmp0, 0);
      emit_local_var_value_write(dstoff, reg_tmp0);
      emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_FALSE);
      emit_local_var_type_write(dstoff, reg_tmp0);
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

      default:
	break;
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
      gen_send_mruby(mrb, m, mid, recv, mrb_class(mrb, recv), status, pc, coi);
    }

    return code;
  }

  const void *
    ent_block_guard(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_irep *irep = *status->irep;
    const void *code = getCurr();

    if (irep->block_lambda) {
      mov(eax, dword [ebx + VMSOffsetOf(irep)]);
      cmp(eax, (Xbyak::uint32)irep);
      jz("@f");
      inLocalLabel();

      gen_flush_regs(mrb, *status->pc, status, coi, 1);

      L(".exitlab");
      mov(eax, dword [eax + OffsetOf(mrb_irep, iseq)]);
      mov(dword [ebx + VMSOffsetOf(pc)], eax);
      emit_load_literal(reg_tmp0, 0);
      mov(edx, ".exitlab");
      ret();

      outLocalLabel();
      L("@@");
    }

    return code;
  }

  const void *
    ent_call(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_value recv = mrb->c->stack[0];
    struct RProc *m = mrb_proc_ptr(recv);
    
    if (MRB_PROC_CFUNC_P(m)) {
      return NULL;
    }

    if (m->body.irep == NULL) {
      return NULL;
    }

    mov(eax, dword [ecx + OffsetOf(mrb_value, value.p)]);
    mov(eax, dword [eax + OffsetOf(struct RProc, body.irep)]);
    mov(eax, dword [eax + OffsetOf(mrb_irep, jit_top_entry)]);
    test(eax, eax);
    push(eax);
    jnz("@f");
    pop(eax);
    gen_exit(mrb, *status->pc, 1, 1, status, coi);
    L("@@");

    emit_cfunc_start();

    lea(eax, dword [ebx + VMSOffsetOf(status)]);
    push(eax);
    push(esi);
    call((void *)mrbjit_exec_call);

    emit_cfunc_end(2 * sizeof(void *));
    ret();

    return code;
  }

  const void *
    ent_enter(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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

    mrb->compile_info.force_compile = 0;
    return code;
  }

  const void *
    ent_return(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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

#if 0
    mrbjit_reginfo *rinfo = &coi->reginfo[GETARG_A(i)];
    mrb_value sclass = mrb_obj_value(mrb_obj_class(mrb, regs[0]));
    printf("%s#%s -> ", 
	   RSTRING_PTR(mrb_funcall(mrb, sclass, "inspect", 0)), 
	   mrb_sym2name(mrb, mrb->c->ci->mid));
    disp_type(mrb, rinfo);
#endif

    gen_flush_regs(mrb, pc, status, coi, 1);

    inLocalLabel();

    /* Set return address from callinfo */
    mov(edx, dword [edi + OffsetOf(mrb_context, ci)]);
    mov(eax, dword [edx + OffsetOf(mrb_callinfo, jit_entry)]);
    test(eax, eax);
    push(eax);
    jnz("@f");
    L(".ret_vm");
    pop(eax);
    gen_exit(mrb, *status->pc, 1, 0, status, coi);
    L("@@");
    
    if (can_inline) {
      /* Check must copy env */
      mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);
      mov(eax, dword [eax + OffsetOf(mrb_callinfo, env)]);
      test(eax, eax);
      jnz(".ret_vm");

      /* Check exception happened? */
      mov(eax, dword [esi + OffsetOf(mrb_state, exc)]);
      test(eax, eax);
      jnz(".ret_vm");

      /* Inline else part of mrbjit_exec_return_fast (but not ensure call) */
      push(edi);

      mov(edi, edx);


      /* Save return value */
      emit_local_var_read(reg_dtmp0, GETARG_A(i) * sizeof(mrb_value));
      /* Store return value (bottom of stack always return space) */
      emit_local_var_write(0, reg_dtmp0);

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

      emit_cfunc_start();

      lea(eax, dword [ebx + VMSOffsetOf(status)]);
      push(eax);
      push(esi);
      if (can_use_fast) {
	call((void *)mrbjit_exec_return_fast);
      }
      else {
	call((void *)mrbjit_exec_return);
      }
      emit_cfunc_end(2 * 4);

      mov(ecx, dword [ebx + VMSOffsetOf(regs)]);

      test(eax, eax);
      jz("@f");
      pop(edx);			/* pop return address from callinfo */
      gen_exit(mrb, NULL, 0, 0, status, coi);
      L("@@");

      ret();
    }

    outLocalLabel();

    return code;
  }

  const void *
    ent_return_inline(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code *pc = *status->pc;

#if 0
    mrb_value *regs = *status->regs;
    mrb_code i = *pc;
    mrbjit_reginfo *rinfo = &coi->reginfo[GETARG_A(i)];
    mrb_value sclass = mrb_obj_value(mrb_obj_class(mrb, regs[0]));
    printf("%s#%s -> ", 
	   RSTRING_PTR(mrb_funcall(mrb, sclass, "inspect", 0)), 
	   mrb_sym2name(mrb, mrb->c->ci->mid));
    disp_type(mrb, rinfo);
#endif

    gen_flush_regs(mrb, pc, status, coi, 1);
    CALL_CFUNC_BEGIN;
    CALL_CFUNC_STATUS(mrbjit_exec_return, 0);

    mov(ecx, dword [ebx + VMSOffsetOf(regs)]);

    return code;
  }

  const void *
    ent_blkpush(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
      /* A Bx   R(A) := block (16=6:1:5:4) */
    mrb_code *pc = *status->pc;
    int i = *pc;
    int a = GETARG_A(i);
    int bx = GETARG_Bx(i);
    int m1 = (bx>>10)&0x3f;
    int r  = (bx>>9)&0x1;
    int m2 = (bx>>4)&0x1f;
    int lv = (bx>>0)&0xf;

    if (lv == 0) {
      emit_local_var_read(reg_dtmp0, (m1 + r + m2 + 1) * sizeof(mrb_value));
      emit_local_var_write(a * sizeof(mrb_value), reg_dtmp0);
    }
    else {
      int i;

      mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);
      mov(eax, dword [eax + OffsetOf(mrb_callinfo, proc)]);
      mov(eax, dword [eax + OffsetOf(struct RProc, env)]);
      for (i = 0; i < lv - 1; i++) {
	mov(eax, dword [eax + OffsetOf(struct REnv, c)]);
      }
      mov(eax, dword [eax + OffsetOf(struct REnv, stack)]);
      movsd(xmm0, ptr [eax + (m1 + r + m2 + 1) * sizeof(mrb_value)]);
      emit_local_var_write(a * sizeof(mrb_value), reg_dtmp0);
    }

    return code;
  }

#define OVERFLOW_CHECK_GEN(AINSTF)                                      \
    jno("@f");                                                          \
    cvtsi2sd(xmm0, dword [ecx + reg0off]);                              \
    cvtsi2sd(xmm1, dword [ecx + reg1off]);				\
    AINSTF(xmm0, xmm1);                                                 \
    emit_local_var_write(reg0off, reg_dtmp0);				\
    gen_exit(mrb, *status->pc + 1, 1, 1, status, coi);			\
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
    dinfo->regplace = MRBJIT_REG_MEMORY;                                \
    dinfo->unboxedp = 0;                                                \
\
    if (r0type == MRB_TT_FIXNUM && r1type == MRB_TT_FIXNUM) {           \
      gen_type_guard(mrb, reg0pos, status, *ppc, coi);			\
      gen_type_guard(mrb, reg1pos, status, *ppc, coi);			\
\
      emit_local_var_value_read(reg_tmp0, reg0off);			\
      AINSTI(eax, dword [ecx + reg1off]);			        \
      OVERFLOW_CHECK_GEN(AINSTF);                                       \
      emit_local_var_value_write(reg0off, reg_tmp0);			\
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
	emit_local_var_read(reg_dtmp0, reg0off);			\
      }                                                                 \
\
      if (r1type == MRB_TT_FIXNUM) {                                    \
        cvtsi2sd(xmm1, dword [ecx + reg1off]);                          \
      }                                                                 \
      else {                                                            \
	emit_local_var_read(reg_dtmp1, reg1off);			\
      }                                                                 \
\
      AINSTF(xmm0, xmm1);				                \
      emit_local_var_write(reg0off, reg_dtmp0);				\
      dinfo->type = MRB_TT_FLOAT;                                       \
      dinfo->klass = mrb->float_class;                                  \
    }                                                                   \
    else {                                                              \
      return ent_send(mrb, status, coi);				\
    }                                                                   \
} while(0)

  const void *
    ent_add(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(add, addsd);
    return code;
  }

  const void *
    ent_sub(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(sub, subsd);
    return code;
  }

  const void *
    ent_mul(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(imul, mulsd);
    return code;
  }

  const void *
    ent_div(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
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
      emit_local_var_read(reg_dtmp0, reg0off);
    }

    if (r1type == MRB_TT_FIXNUM) {
      cvtsi2sd(xmm1, dword [ecx + reg1off]);
    }
    else {
      emit_local_var_read(reg_dtmp1, reg1off);
    }

    divsd(xmm0, xmm1);
    emit_local_var_write(reg0off, reg_dtmp0);

    /* Div returns Float always */
    /* see http://qiita.com/monamour555/items/bcef9b41a5cc4670675a */
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return code;
  }

#define OVERFLOW_CHECK_I_GEN(AINSTF)                                    \
    jno("@f");                                                          \
    cvtsi2sd(xmm0, dword [ecx + off]);                                  \
    emit_load_literal(reg_tmp0, y);					\
    cvtsi2sd(xmm1, eax);                                                \
    AINSTF(xmm0, xmm1);                                                 \
    emit_local_var_write(off, reg_dtmp0);				\
    gen_exit(mrb, *status->pc + 1, 1, 1, status, coi);			\
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
      emit_local_var_value_read(reg_tmp0, off);				\
      AINSTI(eax, y);                                                   \
      OVERFLOW_CHECK_I_GEN(AINSTF);                                     \
      emit_local_var_value_write(off, reg_tmp0);			\
      dinfo->type = MRB_TT_FIXNUM;       				\
      dinfo->klass = mrb->fixnum_class; 				\
    }                                                                   \
    else if (atype == MRB_TT_FLOAT) {					\
      emit_local_var_read(reg_dtmp0, off);				\
      emit_load_literal(reg_tmp0, y);					\
      cvtsi2sd(xmm1, eax);                                              \
      AINSTF(xmm0, xmm1);                                               \
      emit_local_var_write(off, reg_dtmp0);				\
      dinfo->type = MRB_TT_FLOAT;					\
      dinfo->klass = mrb->float_class;  				\
    }                                                                   \
    else {                                                              \
      gen_exit(mrb, *ppc, 1, 0, status, coi);				\
    }                                                                   \
} while(0)
    
  const void *
    ent_addi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_I_GEN(add, addsd);
    return code;
  }

  const void *
    ent_subi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_I_GEN(sub, subsd);
    return code;
  }

#define COMP_GEN_II(CMPINST)                                         \
do {                                                                 \
    emit_local_var_value_read(reg_tmp0, off0);			     \
    cmp(eax, dword [ecx + off1]);                                    \
    CMPINST;     						     \
} while(0)

#define COMP_GEN_IF(CMPINST)                                         \
do {                                                                 \
    cvtsi2sd(xmm0, ptr [ecx + off0]);                                \
    emit_load_literal(reg_tmp0, 0);				     \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST;    						     \
} while(0)

#define COMP_GEN_FI(CMPINST)                                         \
do {                                                                 \
    emit_local_var_read(reg_dtmp0, off0);			     \
    cvtsi2sd(xmm1, ptr [ecx + off1]);                                \
    emit_load_literal(reg_tmp0, 0);				     \
    comisd(xmm0, xmm1);     			                     \
    CMPINST;     						     \
} while(0)

#define COMP_GEN_FF(CMPINST)                                         \
do {                                                                 \
    emit_local_var_read(reg_dtmp0, off0);			     \
    emit_load_literal(reg_tmp0, 0);				     \
    comisd(xmm0, ptr [ecx + off1]);				     \
    CMPINST;    						     \
} while(0)
    
#define COMP_GEN_SS(CMPINST)                                         \
do {                                                                 \
    emit_cfunc_start();						     \
    emit_local_var_type_read(reg_tmp0, off1);			     \
    push(eax);                                                       \
    emit_local_var_value_read(reg_tmp0, off1);			     \
    push(eax);                                                       \
    emit_local_var_type_read(reg_tmp0, off0);			     \
    push(eax);                                                       \
    emit_local_var_value_read(reg_tmp0, off0);			     \
    push(eax);                                                       \
    push(esi);                                                       \
    call((void *)mrb_str_cmp);                                       \
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(mrb_value) * 2);     \
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
    else if (mrb_type(regs[regno]) == MRB_TT_FALSE &&                \
             mrb_type(regs[regno + 1]) == MRB_TT_FALSE) {            \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_II(CMPINSTI);                                     \
    }                                                                \
    else {                                                           \
      return ent_send(mrb, status, coi);				     \
    }                                                                \
 } while(0)

#define COMP_BOOL_SET                                                \
do {								     \
    xor(ah, ah);                                                     \
    cwde();                                                          \
    add(eax, eax);                                                   \
    or(eax, 0xfff00001);                                             \
    emit_local_var_type_write(off0, reg_tmp0);			     \
  } while(0)

#define COMP_GEN(CMPINSTI, CMPINSTF)			             \
do {                                                                 \
    int regno = GETARG_A(**ppc);	                             \
    const Xbyak::uint32 off0 = regno * sizeof(mrb_value);            \
    const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);             \
                                                                     \
    COMP_GEN_CMP(CMPINSTI, CMPINSTF);                                \
    COMP_BOOL_SET;                                                   \
} while(0)

#define COMP_GEN_JMP(CMPINSTI, CMPINSTF)                 	     \
do {                                                                 \
    int regno = GETARG_A(**ppc);	                             \
    const Xbyak::uint32 off0 = regno * sizeof(mrb_value);            \
    const Xbyak::uint32 off1 = off0 + sizeof(mrb_value);             \
                                                                     \
    COMP_GEN_CMP(CMPINSTI, CMPINSTF);                                \
 } while(0)

  const void *
    ent_eq(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int regno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[regno];

#if 1
    COMP_GEN_JMP(setz(al), setz(al));
    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_AL;
    dinfo->unboxedp = 1;
    return code;
#endif
      
    COMP_GEN(setz(al), setz(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_lt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int regno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[regno];

#if 1
    COMP_GEN_JMP(setl(al), setb(al));
    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_AL;
    dinfo->unboxedp = 1;
    return code;
#endif

    COMP_GEN(setl(al), setb(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_le(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

#if 1
    COMP_GEN_JMP(setle(al), setbe(al));
    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_AL;
    dinfo->unboxedp = 1;
    return code;
#endif

    COMP_GEN(setle(al), setbe(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_gt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

#if 1
    COMP_GEN_JMP(setg(al), seta(al));
    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_AL;
    dinfo->unboxedp = 1;
    return code;
#endif

    COMP_GEN(setg(al), seta(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_ge(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

#if 1
    COMP_GEN_JMP(setge(al), setae(al));
    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_AL;
    dinfo->unboxedp = 1;
    return code;
#endif

    COMP_GEN(setge(al), setae(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_array(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    int siz = GETARG_C(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    mov(eax, ptr [esi + OffsetOf(mrb_state, arena_idx)]);
    push(eax);
    emit_cfunc_start();

    lea(eax, ptr [ecx + srcoff]);
    push(eax);
    emit_load_literal(reg_tmp0, siz);
    push(eax);
    push(esi);
    call((void *) mrb_ary_new_from_values);
    
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(int) + sizeof(mrb_value *));

    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);

    pop(eax);
    mov(ptr [esi + OffsetOf(mrb_state, arena_idx)], eax);

    dinfo->type = MRB_TT_ARRAY;
    dinfo->klass = mrb->array_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_arycat(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    mov(eax, ptr [esi + OffsetOf(mrb_state, arena_idx)]);
    push(eax);

    emit_cfunc_start();

    emit_local_var_type_read(reg_tmp0, srcoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff);
    push(eax);
    push(esi);
    call((void *) mrb_ary_splat);

    emit_cfunc_end(sizeof(mrb_state *) + sizeof(mrb_value));

    emit_cfunc_start();

    /* rc of splat */
    push(edx);
    push(eax);
    /* arg1 reg */
    emit_local_var_type_read(reg_tmp0, dstoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, dstoff);
    push(eax);
    /* mrb */
    push(esi);
    call((void *) mrb_ary_concat);
    
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(mrb_value) * 2);

    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);

    pop(eax);
    mov(ptr [esi + OffsetOf(mrb_state, arena_idx)], eax);

    dinfo->type = MRB_TT_ARRAY;
    dinfo->klass = mrb->array_class;
    dinfo->constp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_getupvar(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    mov(eax, dword [edi + OffsetOf(mrb_context, ci)]);
    mov(eax, dword [eax + OffsetOf(mrb_callinfo, proc)]);
    mov(eax, dword [eax + OffsetOf(struct RProc, env)]);
    for (i = 0; i < uppos; i++) {
      mov(eax, dword [eax + OffsetOf(struct REnv, c)]);
    }
    mov(eax, dword [eax + OffsetOf(struct REnv, stack)]);

    movsd(xmm0, ptr [eax + idxpos * sizeof(mrb_value)]);
    emit_local_var_write(dstoff, reg_dtmp0);

    return code;
  }

  const void *
    ent_setupvar(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
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

    emit_local_var_read(reg_dtmp0, valoff);
    movsd(ptr [eax + idxpos * sizeof(mrb_value)], xmm0);

    return code;
  }

  const void *
    ent_jmp(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    return code;
  }

  const void *
    ent_jmpif(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int cond = GETARG_A(**ppc);
    const Xbyak::uint32 off0 =  cond * sizeof(mrb_value);
    mrbjit_reginfo *rinfo = &coi->reginfo[cond];

#if 1
    if (rinfo->regplace == MRBJIT_REG_AL) {
      test(al, al);
      if (mrb_test(regs[cond])) {
	jnz("@f");
	gen_exit(mrb, *ppc + 1, 1, 0, status, coi);
	L("@@");
      }
      else {
	jz("@f");
	gen_exit(mrb, *ppc + GETARG_sBx(**ppc), 1, 0, status, coi);
	L("@@");
      }

      return code;
    }
#endif
    
    emit_local_var_type_read(reg_tmp0, off0);
    if (mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 1, cond, *ppc + 1, status, coi);
    }
    else {
      gen_bool_guard(mrb, 0, cond, *ppc + GETARG_sBx(**ppc), status, coi);
    }

    return code;
  }

  const void *
    ent_jmpnot(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int cond = GETARG_A(**ppc);
    const Xbyak::uint32 off0 =  cond * sizeof(mrb_value);
    mrbjit_reginfo *rinfo = &coi->reginfo[cond];

#if 1
    if (rinfo->regplace == MRBJIT_REG_AL) {
      test(al, al);
      if (!mrb_test(regs[cond])) {
	jz("@f");
	gen_exit(mrb, *ppc + 1, 1, 0, status, coi);
	L("@@");
      }
      else {
	jnz("@f");
	gen_exit(mrb, *ppc + GETARG_sBx(**ppc), 1, 0, status, coi);
	L("@@");
      }

      return code;
    }
#endif

    emit_local_var_type_read(reg_tmp0, off0);
    if (!mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 0, cond, *ppc + 1, status, coi);
    }
    else {
      gen_bool_guard(mrb, 1, cond, *ppc + GETARG_sBx(**ppc), status, coi);
    }

    return code;
  }

  const void *
    ent_lambda(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    if (mirep->simple_lambda && c->proc_pool) {
      for (i = -1; c->proc_pool[i].proc.tt == MRB_TT_PROC; i--) {
	if (c->proc_pool[i].proc.body.irep == mirep) {
	  struct RProc *nproc = &c->proc_pool[i].proc;
	  emit_load_literal(reg_tmp0, (Xbyak::uint32)nproc);
	  emit_local_var_value_write(dstoff, reg_tmp0);
	  emit_load_literal(reg_tmp0, 0xfff00000 | MRB_TT_PROC);
	  emit_local_var_type_write(dstoff, reg_tmp0);
	  /* mov(edx, (Xbyak::uint32)nproc->env);
	     mov(dword [edx + OffsetOf(struct REnv, stack)], ecx);
	     mov(eax, dword [esi + OffsetOf(mrb_state, c)]);
	     mov(eax, dword [eax + OffsetOf(mrb_context, ci)]);
	     mov(eax, dword [eax + OffsetOf(mrb_callinfo, proc)]);
	     mov(eax, dword [eax + OffsetOf(struct RProc, env)]);
	     mov(dword [edx + OffsetOf(struct REnv, c)], eax); */
	  /*
	          emit_cfunc_start();
		  emit_local_var_type_read(reg_tmp0, dstoff);
		  push(eax);
		  emit_local_var_value_read(reg_tmp0, dstoff);
		  push(eax);
		  push(esi);
		  call((void *)mrb_p);
		  emit_cfunc_end(12);
	  */
	  return code;
	}
      }
    }

    mov(eax, ptr [esi + OffsetOf(mrb_state, arena_idx)]);
    push(eax);
    emit_cfunc_start();
    emit_load_literal(reg_tmp0, (Xbyak::uint32)mirep);
    push(eax);
    push(esi);
    if (flags & OP_L_CAPTURE) {
      call((void *) mrb_closure_new);
    }
    else {
      call((void *) mrb_proc_new);
    }
    emit_cfunc_end(2 * sizeof(void *));
    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_load_literal(reg_tmp1, 0xfff00000 | MRB_TT_PROC);
    emit_local_var_type_write(dstoff, reg_tmp1);
    if (flags & OP_L_STRICT) {
      emit_load_literal(reg_tmp1, (Xbyak::uint32)MRB_PROC_STRICT);
      shl(edx, 11);
      or(ptr [eax], edx);
    }
    pop(eax);
    mov(ptr [esi + OffsetOf(mrb_state, arena_idx)], eax);
    return code;
  }

  const void *
    ent_range(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
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
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_cfunc_start();

    emit_load_literal(reg_tmp0, exelp);
    push(eax);
    emit_local_var_type_read(reg_tmp0, srcoff1);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff1);
    push(eax);
    emit_local_var_type_read(reg_tmp0, srcoff0);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff0);
    push(eax);
    push(esi);
    call((void *) mrb_range_new);
    
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(mrb_value) * 2 + sizeof(int));

    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);
    return code;
  }

  const void *
    ent_string(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrb_irep *irep = *status->irep;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    mrb_value *str = irep->pool + GETARG_Bx(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_STRING;
    dinfo->klass = mrb->string_class;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_cfunc_start();

    emit_load_literal(reg_tmp0, (Xbyak::uint32)str);
    mov(edx, dword [eax + 4]);
    push(edx);
    mov(edx, dword [eax]);
    push(edx);
    push(esi);
    call((void *) mrb_str_dup);
    
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(mrb_value));

    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);

    return code;
  }

  const void *
    ent_strcat(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_STRING;
    dinfo->klass = mrb->string_class;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    push(ecx);
    push(ebx);

    emit_local_var_type_read(reg_tmp0, srcoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, srcoff);
    push(eax);
    emit_local_var_type_read(reg_tmp0, dstoff);
    push(eax);
    emit_local_var_value_read(reg_tmp0, dstoff);
    push(eax);
    push(esi);
    call((void *) mrb_str_concat);
    add(esp, sizeof(mrb_state *) + sizeof(mrb_value) * 2);
    
    pop(ebx);
    pop(ecx);

    return code;
  }

  const void *
    ent_hash(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstoff = GETARG_A(**ppc) * sizeof(mrb_value);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    int num = GETARG_C(**ppc);
    int i;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_HASH;
    dinfo->klass = mrb->hash_class;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    dinfo->unboxedp = 0;

    emit_cfunc_start();

    push((Xbyak::uint32)num);
    push(esi);
    call((void *) mrb_hash_new_capa);
    emit_cfunc_end(sizeof(mrb_state *) + sizeof(int));

    for(i = 0; i < num * 2; i+= 2) {
      push(eax);
      push(edx);
      emit_cfunc_start();

      /* val */
      emit_local_var_type_read(reg_vars, srcoff + (i + 1) * sizeof(mrb_value));
      push(ebx);
      emit_local_var_value_read(reg_vars, srcoff + (i + 1) * sizeof(mrb_value));
      push(ebx);

      /* key */
      emit_local_var_type_read(reg_vars, srcoff + i * sizeof(mrb_value));
      push(ebx);
      emit_local_var_value_read(reg_vars, srcoff + i * sizeof(mrb_value));
      push(ebx);

      /* hash */
      push(edx);
      push(eax);

      /* mrb */
      push(esi);

      call((void *)mrb_hash_set);

      emit_cfunc_end(sizeof(mrb_state *) + sizeof(mrb_value) * 3);
      pop(edx);
      pop(eax);
    }

    emit_local_var_value_write(dstoff, reg_tmp0);
    emit_local_var_type_write(dstoff, reg_tmp1);

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
  const void *
    mrbjit_prim_obj_not_equal_aux(mrb_state *mrb, mrb_value proc,
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
    mrbjit_prim_mmm_instance_new_impl(mrb_state *mrb, mrb_value proc,
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

/*
** mruby/jitcode.h - Class for XBYAK
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JITCOD_H
#define MRUBY_JITCODE_H

#ifdef __i386__
#include "jit_x86.h"
#elif __x86_64__
#include "jit_x64.h"
#endif

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
void *mrbjit_exec_send_c_void(mrb_state *, mrbjit_vmstatus *, 
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

    case MRBJIT_REG_IMMIDATE:
      gen_flush_literal(mrb, coi, pos);
      /* Keep regplace for guard modify in guard fail */
      coi->reginfo[pos].regplace = MRBJIT_REG_IMMIDATE;
      break;

    case MRBJIT_REG_AL:
      /*      printf("%x %x \n", *status->pc, *status->irep);
      disasm_irep(mrb, *status->irep);
      disasm_once(mrb, *status->irep, **status->pc);*/
      //printf("\n\n");
      /*coi->reginfo[pos].regplace = MRBJIT_REG_MEMORY;
      emit_bool_boxing(mrb, coi, reg_tmp0);
      emit_local_var_type_write(mrb, coi, pos, reg_tmp0);
      coi->reginfo[pos].regplace = MRBJIT_REG_AL;*/
      break;
       
    case MRBJIT_REG_XMM0:
    case MRBJIT_REG_XMM1:
    case MRBJIT_REG_XMM2:
    case MRBJIT_REG_XMM3:
    case MRBJIT_REG_XMM4:
    case MRBJIT_REG_XMM5:
    case MRBJIT_REG_XMM6:
    case MRBJIT_REG_XMM7:
      {
	gen_flush_xmm(mrb, coi, pos);
      }
      break;

    case MRBJIT_REG_VMREG0:
    case MRBJIT_REG_VMREG1:
    case MRBJIT_REG_VMREG2:
    case MRBJIT_REG_VMREG3:
    case MRBJIT_REG_VMREG4:
    case MRBJIT_REG_VMREG5:
    case MRBJIT_REG_VMREG6:
    case MRBJIT_REG_VMREG7:
      {
	enum mrbjit_regplace regp;
	regp = coi->reginfo[pos].regplace;
	emit_local_var_read(mrb, coi, reg_dtmp0, regp - MRBJIT_REG_VMREG0);
	emit_local_var_write(mrb, coi, pos, reg_dtmp0);
        /* regplace is changed in emit_local_var_write */
	coi->reginfo[pos].regplace = regp;
      }
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
    unsigned int i;

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
    case MRBJIT_REG_IMMIDATE:
    case MRBJIT_REG_MEMORY:
      break;

    case MRBJIT_REG_AL:
      /*printf("%x %x \n", *status->pc, *status->irep);
      disasm_irep(mrb, *status->irep);
      disasm_once(mrb, *status->irep, **status->pc);*/
    //printf("\n\n");
      /*      coi->reginfo[pos].regplace = MRBJIT_REG_MEMORY;
      emit_local_var_type_read(mrb, coi, reg_tmp0s, pos);
      emit_cmp(mrb, coi, reg_tmp0s, 0xfff00001);
      setnz(al);
      coi->reginfo[pos].regplace = MRBJIT_REG_AL;*/
      break;
       
    case MRBJIT_REG_XMM0:
    case MRBJIT_REG_XMM1:
    case MRBJIT_REG_XMM2:
    case MRBJIT_REG_XMM3:
    case MRBJIT_REG_XMM4:
    case MRBJIT_REG_XMM5:
    case MRBJIT_REG_XMM6:
    case MRBJIT_REG_XMM7:
      break;

    case MRBJIT_REG_VMREG0:
    case MRBJIT_REG_VMREG1:
    case MRBJIT_REG_VMREG2:
    case MRBJIT_REG_VMREG3:
    case MRBJIT_REG_VMREG4:
    case MRBJIT_REG_VMREG5:
    case MRBJIT_REG_VMREG6:
    case MRBJIT_REG_VMREG7:
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
    unsigned int i;

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

    //emit_load_literal(mrb, coi, reg_regs, (cpu_word_t)(*status->pc));
    //emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, prev_pc), reg_regs);

    if (coi) {
      mrb_irep *irep = *status->irep;
      int ioff = ISEQ_OFFSET_OF(*status->pc);
      int toff = coi - (irep->jit_entry_tab + ioff)->body;

      /* break ecx(reg_regs) don't use for this aim */
      emit_move(mrb, coi, reg_tmp1, reg_context,  OffsetOf(mrb_context, ci));
      emit_load_literal(mrb, coi, reg_regs, toff);
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, prev_tentry_offset), reg_regs);

      assert(coi->method_arg_ver < 0x8000);
      if (!is_clr_rc) {
	emit_push(mrb, coi, reg_tmp0);
      }
      emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
      emit_load_literal(mrb, coi, reg_tmp0, coi->method_arg_ver);
      emit_movew(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, method_arg_ver), ax);
      if (!is_clr_rc) {
	emit_pop(mrb, coi, reg_tmp0);
      }
    }

    if (pc) {
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)pc);
    }
    if (is_clr_rc == 2) {
      emit_load_literal(mrb, coi, reg_tmp0, 1); /* Top method type guard fail */
    }
    else if (is_clr_rc == 3) {
      /* JMPIF/ JMPNOT fail */
      emit_load_literal(mrb, coi, reg_tmp0, 3);
    }
    else if (is_clr_rc) {
      emit_load_literal(mrb, coi, reg_tmp0, 0);
    }

    if (is_clr_exitpos == 1) {
      emit_load_literal(mrb, coi, reg_tmp1, 0);
    }
    else if (is_clr_exitpos == 2 && (*status->irep)->may_overflow == 0) {
      emit_load_literal(mrb, coi, reg_tmp1, 1); /* Arthmetic overflow */
    }
    else {
      //mov(reg_tmp1, (cpu_word_t)exit_ptr);
      emit_load_label(mrb, coi, reg_tmp1, ".exitlab");
    }
    ret();
    mrb->compile_info.nest_level = 0;
    outLocalLabel();
  }
  
  const void *
    gen_jump_block(mrb_state *mrb, void *entry, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrbjit_code_info *prevcoi)
  {
    const void *code = getCurr();
    mrb_irep *irep = *status->irep;
    unsigned int i;

    if (coi && prevcoi && coi->reginfo && prevcoi->reginfo) {
      for (i = 0; i < irep->nregs; i++) {
	if (coi->reginfo[i].regplace != prevcoi->reginfo[i].regplace) {
	  gen_flush_one(mrb, status, prevcoi, i);
	  gen_restore_one(mrb, status, coi, i);
	}
      }
    }
    else {
      gen_flush_regs(mrb, *status->pc, status, prevcoi, 1);
      gen_restore_regs(mrb, *status->pc, status, coi);
    }

    emit_jmp(mrb, coi, entry);

    return code;
  }

  void 
    gen_jmp_patch(mrb_state *mrb, void *dst, void *target, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    emit_jmp(mrb, coi, target);
    setSize(cursize);
  }

  void 
    gen_load_patch(mrb_state *mrb, void *dst, cpu_word_t address, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    emit_load_literal_noopt(mrb, coi, reg_tmp1, address);
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
    gen_exit_patch2(mrb_state *mrb, void *dst, mrb_code *pc, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    size_t cursize = getSize();
    const unsigned char *code = getCode();
    size_t dstsize = (unsigned char *)dst - code;

    setSize(dstsize);
    emit_call(mrb, coi, (void *)mrbjit_reset_caller);
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
    gen_type_guard(mrb_state *mrb, int regpos, mrbjit_vmstatus *status, mrb_code *pc, mrbjit_code_info *coi)
  {
    enum mrb_vtype tt = (enum mrb_vtype) mrb_type((mrb->c->stack)[regpos]);
    mrbjit_reginfo *rinfo = &coi->reginfo[regpos];
    mrb_irep *irep = *status->irep;

    if (rinfo->type == tt && irep->may_overflow == 0) {
      return;
    }

    if (rinfo->regplace == MRBJIT_REG_IMMIDATE && 
	mrb_type(rinfo->value) == tt) {
      return;
    }

    /* Get type tag */
    emit_local_var_type_read(mrb, coi, reg_tmp0s, regpos);
    rinfo->type = tt;
    rinfo->klass = mrb_class(mrb, (mrb->c->stack)[regpos]);
    if (rinfo->regplace > MRBJIT_REG_VMREG0) {
      int orgno = rinfo->regplace - MRBJIT_REG_VMREG0;
      mrbjit_reginfo *oinfo = &coi->reginfo[orgno];
      oinfo->type = tt;
      oinfo->klass = mrb_class(mrb, (mrb->c->stack)[regpos]);
    }
    /* Input reg_tmp0 for type tag */
    if (tt == MRB_TT_FLOAT) {
      emit_cmp(mrb, coi, reg_tmp0s, 0xfff00000);
      jb("@f", T_NEAR);
      emit_cmp(mrb, coi, reg_tmp0s, 0xfff80000);
      jz("@f", T_NEAR);
    } 
    else {
      emit_cmp(mrb, coi, reg_tmp0s, 0xfff00000 | tt);
      jz("@f", T_NEAR);
    }

    /* Guard fail exit code */
    gen_exit(mrb, pc, 2, 0, status, coi);

    L("@@");
  }

  /*
   input REG_TMP0 Pointer to tested boolean
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

    emit_cmp(mrb, coi, reg_tmp0s, 0xfff00001);
    if (b) {
      jnz("@f", T_NEAR);
    } 
    else {
      jz("@f", T_NEAR);
    }

    /* Guard fail exit code */
    gen_exit(mrb, pc, 3, 0, status, coi);

    L("@@");
  }

  /* Check current object blong to class. Difference of type guard is 
   this guard chaeck obj->c when v is normal object.
     destroy REG_TMP0
  */
  void 
    gen_class_guard(mrb_state *mrb, int regpos, mrbjit_vmstatus *status, mrb_code *pc, mrbjit_code_info *coi, struct RClass *c, int rc)
  {
    enum mrb_vtype tt;
    mrb_value v = (mrb->c->stack)[regpos];
    mrbjit_reginfo *rinfo = &coi->reginfo[regpos];
    const void *exitpos = NULL;

    inLocalLabel();
    tt = (enum mrb_vtype)mrb_type(v);

    if (rinfo->regplace > MRBJIT_REG_VMREG0) {
      int orgno = rinfo->regplace - MRBJIT_REG_VMREG0;
      mrbjit_reginfo *oinfo = &coi->reginfo[orgno];

      *rinfo = *oinfo;
      rinfo->regplace = (enum mrbjit_regplace)(MRBJIT_REG_VMREG0 + orgno);
    }

    if (rinfo->type != tt || !rinfo->constp) {

      rinfo->type = tt;
      emit_local_var_type_read(mrb, coi, reg_tmp0s, regpos);

      if (tt == MRB_TT_FLOAT) {
	emit_cmp(mrb, coi, reg_tmp0s, 0xfff00000);
	jb("@f", T_NEAR);
	emit_cmp(mrb, coi, reg_tmp0s, 0xfff80000);
	jz("@f", T_NEAR);
      }
      else {
	emit_cmp(mrb, coi, reg_tmp0s, 0xfff00000 | tt);
	jz("@f", T_NEAR);
      }

      /* Guard fail exit code */
      exitpos = getCurr();
      L(".exitpos");
      gen_exit(mrb, pc, 2, 0, status, coi);

      L("@@");
    }

    /* Import from class.h */
    switch (tt) {
    case MRB_TT_FALSE:
      emit_local_var_value_read(mrb, coi, reg_tmp0s, regpos);
      emit_cmp(mrb, coi, reg_tmp0s, (cpu_word_t)v.value.i);
      /* Guard fail exit code */
      if (exitpos)  {
	jnz(".exitpos");
      }
      else {
	jz("@f", T_NEAR);
	gen_exit(mrb, pc, 2, 0, status, coi);
      }

      L("@@");
      break;

    case MRB_TT_TRUE:
    case MRB_TT_SYMBOL:
    case MRB_TT_FIXNUM:
    case MRB_TT_FLOAT:
      //case MRB_TT_ARRAY:
    case MRB_TT_STRING:
    case MRB_TT_HASH:
    case MRB_TT_RANGE:
    case MRB_TT_PROC:
    case MRB_TT_EXCEPTION:
    case MRB_TT_FILE:
      /* DO NOTHING */
      break;

    default:
      {
	if (c == NULL) {
	  c = mrb_obj_ptr(v)->c;
	}
	if (rinfo->klass == c && rc != 2) {
	  return;
	}
	rinfo->klass = c;
	emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, regpos);
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RBasic, c));
	emit_cmp(mrb, coi, reg_tmp0, (cpu_word_t)c);
	/* Guard fail exit code */
	if (exitpos)  {
	  jnz(".exitpos");
	}
	else {
	  jz("@f", T_NEAR);
	  gen_exit(mrb, pc, 2, 0, status, coi);
	}

	L("@@");
      }
      break;
    }
    outLocalLabel();
  }
  
  void
    gen_lvar_get(const Xbyak::Mmx& dst, int no, mrbjit_code_info *coi)
  {
    
  }

  void 
    gen_set_jit_entry(mrb_state *mrb, mrb_code *pc, mrbjit_code_info *coi, mrb_irep *irep)
  {
    unsigned int ioff;
    int toff;
    mrbjit_codetab *ctab;

    ioff = ISEQ_OFFSET_OF(pc);
    if (irep->ilen <= ioff + 1) {
      /* Maybe tailcall */
      return;
    }
    toff = coi - (irep->jit_entry_tab + ioff)->body;

    /* Check and grow code table */
    ctab = (irep->jit_entry_tab + ioff + 1);
    if (ctab->size <= toff) {
      int oldsize;
      int j;

      oldsize = ctab->size;
      ctab->size = toff + (toff >> 1) + 2;
      ctab->body = (mrbjit_code_info *)mrb_realloc(mrb, ctab->body, 
						   sizeof(mrbjit_code_info) * ctab->size);
      for (j = oldsize; j < ctab->size; j++) {
        ctab->body[j].used = 0;
        ctab->body[j].reginfo = NULL;
        ctab->body[j].patch_pos = NULL;
        ctab->body[j].entry = NULL;
      }
    }

    /* This code is patched when compiled continaution code */
    if (ctab->body[toff].entry) {
      emit_load_literal_noopt(mrb, coi, reg_tmp1, 
			      (cpu_word_t)ctab->body[toff].entry);
    }
    else {
      ctab->body[toff].patch_pos = getCurr();
      emit_load_literal_noopt(mrb, coi, reg_tmp1, 0);
    }

    //ci->jit_entry = (irep->jit_entry_tab[-1] + ioff)->body[0].entry;
    /* reg_context must point current context  */
    emit_move(mrb, coi, reg_tmp0, reg_context,  OffsetOf(mrb_context, ci));

    //printf("%d ", toff);
    emit_move(mrb, coi, reg_tmp0, OffsetOf(mrb_callinfo, jit_entry) - sizeof(mrb_callinfo), reg_tmp1);
  }

#ifdef ENABLE_DEBUG
  void
    gen_call_fetch_hook(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    emit_push(mrb, coi, reg_tmp1);
    emit_push(mrb, coi, reg_tmp0);
    emit_cfunc_start(mrb, coi);
    //emit_arg_push(mrb, coi, 3, reg_regs);
    // load_vm_var_read(reg_tmp0, VMSOffsetOf(pc));
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)(*(status->pc)));
    emit_arg_push(mrb, coi, 2, reg_tmp0);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)(*(status->irep)));
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb->code_fetch_hook);
    emit_cfunc_end(mrb, coi, sizeof(void *) * 4);
    emit_pop(mrb, coi, reg_tmp0);
    emit_pop(mrb, coi, reg_tmp1);
  }
#endif

  void
    gen_stack_extend(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi,
		     cpu_word_t room, cpu_word_t keep, cpu_word_t nlocal)
  {
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, stend));
    emit_sub(mrb, coi, reg_tmp1, room * sizeof(mrb_value));
    emit_cmp(mrb, coi, reg_regs, reg_tmp1);
    jb("@f");

    if (addr_call_stack_extend == NULL) {
      emit_load_label(mrb, coi, reg_tmp0, "@f");
      emit_push(mrb, coi, reg_tmp0);
      emit_load_literal(mrb, coi, reg_tmp1, room);

      addr_call_stack_extend = (void *)getCurr();

      emit_cfunc_start(mrb, coi);
      emit_arg_push(mrb, coi, 1, reg_tmp1);
      emit_arg_push(mrb, coi, 0, reg_mrb);
      emit_call(mrb, coi, (void *) mrbjit_stack_extend);
      emit_cfunc_end(mrb, coi, 2 * sizeof(void *));

      emit_move(mrb, coi, reg_regs, reg_context, OffsetOf(mrb_context, stack));
      ret();
    }
    else {
      emit_load_literal(mrb, coi, reg_tmp1, room);
      emit_load_literal(mrb, coi, reg_tmp0, keep);
      call(addr_call_stack_extend);
    }
      
    L("@@");
  }

  void
    gen_stack_clear(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, cpu_word_t keep, cpu_word_t nlocal)
  {
    unsigned int i;

    /* Registor clear */
    if (keep < nlocal) {
      // printf("%d %d %d \n", keep, room, nlocal);
      emit_load_literal(mrb, coi, reg_tmp0s, 0);
      emit_load_literal(mrb, coi, reg_tmp1s, 0xfff00000 | MRB_TT_FALSE);
      for (i = keep; i < nlocal; i++) {
	emit_local_var_value_write_noopt(mrb, coi, i, reg_tmp0s);
	emit_local_var_type_write(mrb, coi, i, reg_tmp1s);
      }
    }
  }

  void 
    gen_send_mruby(mrb_state *mrb, struct RProc *m, mrb_sym mid, mrb_value recv, 
		   struct RClass *c, mrbjit_vmstatus *status, mrb_code *pc, 
		   mrbjit_code_info *coi)
  {
    int callee_nregs;
    int callee_nlocals;
    mrb_irep *irep = *status->irep;
    int i = *pc;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
    int is_block_call = (m->body.irep->ilen == 1 && 
			 GET_OPCODE(m->body.irep->iseq[0]) == OP_CALL);
    int cisize = 6 + (sizeof(mrb_callinfo) > 64);

    callee_nregs = m->body.irep->nregs;
    callee_nlocals = m->body.irep->nlocals;

    /* Reg map */
    /*    old ci  reg_tmp1 */
    /*    tmp  reg_tmp0 */
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
    emit_add(mrb, coi, reg_tmp1, sizeof(mrb_callinfo) * 2);
    emit_cmp(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ciend));
    jl("@f");

    if (addr_call_extend_callinfo == NULL) {
      emit_load_label(mrb, coi, reg_tmp0, "@f");
      emit_push(mrb, coi, reg_tmp0);

      addr_call_extend_callinfo = (void *)getCurr();

      /* extend cfunction */
      emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
      emit_cfunc_start(mrb, coi);
      emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(mrb_context, cibase));
      emit_sub(mrb, coi, reg_tmp1, reg_tmp0);
      shr(reg_tmp1, cisize);		/* sizeof mrb_callinfo */
      emit_arg_push(mrb, coi, 2, reg_tmp1);
      emit_move(mrb, coi, reg_tmp0, reg_mrb, OffsetOf(mrb_state, c));
      emit_arg_push(mrb, coi, 1, reg_tmp0);
      emit_arg_push(mrb, coi, 0, reg_mrb);
      emit_call(mrb, coi, (void *)mrbjit_exec_extend_callinfo);
      emit_cfunc_end(mrb, coi, 3 * sizeof(void *));
      //emit_move(mrb, coi, reg_context, reg_mrb, OffsetOf(mrb_state, c));
      ret();
    }
    else {
      call(addr_call_extend_callinfo);
    }

    L("@@");
    /*    ci  reg_context */
    /*    tmp  reg_tmp1 */
    /*    tmp  reg_tmp0 */
    emit_move(mrb, coi, reg_context, reg_mrb, OffsetOf(mrb_state, c));
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
    emit_add(mrb, coi, reg_context, OffsetOf(mrb_context, ci), (cpu_word_t)sizeof(mrb_callinfo));
    emit_move(mrb, coi, reg_tmp0s, reg_context, OffsetOf(mrb_context, eidx));
    emit_move(mrb, coi, reg_context, reg_context, OffsetOf(mrb_context, ci));
    /* ci->epos = c->eidx */
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, epos), reg_tmp0s);

    emit_load_literal(mrb, coi, reg_tmp0, 0);
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, env), reg_tmp0);
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, err), reg_tmp0);
    if (irep->jit_inlinep == 0 || 1) {
      //emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, jit_entry), reg_tmp0);
    }

    /* Inherit ridx */
    emit_move(mrb, coi, reg_tmp0s, reg_tmp1, OffsetOf(mrb_callinfo, ridx));
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, ridx), reg_tmp0s);

    if (n == CALL_MAXARGS) {
      emit_load_literal(mrb, coi, reg_tmp0s, -1);
    }
    else {
      emit_load_literal(mrb, coi, reg_tmp0s, n);
    }
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, argc), reg_tmp0s);

    emit_move(mrb, coi, reg_tmp1, reg_mrb, OffsetOf(mrb_state, c));
    emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(mrb_context, stack));
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, stackent), reg_tmp0);

    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)c);
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, target_class), reg_tmp0);

    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)(pc + 1));
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, pc), reg_tmp0);

    if (is_block_call) {
      /* Block call */
      callee_nregs += mrb_proc_ptr(recv)->body.irep->nregs;
      callee_nlocals += mrb_proc_ptr(recv)->body.irep->nlocals;
    }
    else {
      /* normal call */
      emit_moves(mrb, coi, reg_context, OffsetOf(mrb_callinfo, nregs), (uint32_t)m->body.irep->nregs);

      emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, proc), (cpu_word_t)m);

      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), (cpu_word_t)m->body.irep);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), (cpu_word_t)m);
      mrb_gc_register(mrb, mrb_obj_value(m));
    }

    emit_load_literal(mrb, coi, reg_tmp0s, (uint32_t)mid);
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, mid), reg_tmp0s);

    emit_load_literal(mrb, coi, reg_tmp0, -1);
    emit_movew(mrb, coi, reg_context, OffsetOf(mrb_callinfo, prev_tentry_offset), ax);

    emit_load_literal(mrb, coi, reg_tmp0s, (uint32_t)a);
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, acc), reg_tmp0s);

    /*  mrb->c   reg_context  */
    emit_move(mrb, coi, reg_context, reg_mrb, OffsetOf(mrb_state, c));
    shl(reg_tmp0, 3);		/* * sizeof(mrb_value) */
    emit_add(mrb, coi, reg_context, OffsetOf(mrb_context, stack), reg_tmp0);
    emit_move(mrb, coi, reg_regs, reg_context, OffsetOf(mrb_context, stack));
    
    if (!is_block_call) {
      if (n == CALL_MAXARGS) {
	gen_stack_extend(mrb, status, coi, (callee_nregs < 3) ? 3 : callee_nregs, 3,
			 (callee_nlocals < 2) ? 3 : callee_nlocals + 1);
      }
      else {
	gen_stack_extend(mrb, status, coi, callee_nregs, n + 2, callee_nlocals);
      }
    }

    if (irep->jit_inlinep == 0) {
      gen_set_jit_entry(mrb, pc, coi, irep);
    }

    if (is_block_call) {
      emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(mrb_context, ci));
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, 0); /* self */
      emit_move(mrb, coi, reg_tmp0, OffsetOf(mrb_callinfo, proc), reg_tmp1);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp1);

      emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RProc, target_class));
      emit_move(mrb, coi, reg_tmp0, OffsetOf(mrb_callinfo, target_class), reg_tmp1);

      emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, 0); /* self */
      emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RProc, body.irep));
      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), reg_tmp1);

      emit_move(mrb, coi, reg_tmp1s, reg_tmp1, OffsetOf(mrb_irep, nregs));
      emit_move(mrb, coi, reg_tmp0, OffsetOf(mrb_callinfo, nregs), reg_tmp1s);

      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, 0); /* self */
      emit_move(mrb, coi, reg_tmp1, reg_tmp0, OffsetOf(struct RProc, body.irep));
      emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(mrb_irep, iseq));
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), reg_tmp1);

      emit_move(mrb, coi, reg_tmp1, reg_tmp0, OffsetOf(struct RProc, env));
      emit_move(mrb, coi, reg_tmp1, reg_tmp1 , OffsetOf(struct REnv, stack));
      emit_move(mrb, coi, reg_dtmp0, reg_tmp1, 0);
      emit_move(mrb, coi, reg_regs, reg_context, OffsetOf(mrb_context, stack));
      emit_move(mrb, coi, reg_regs, 0, reg_dtmp0);

      mrb->compile_info.force_compile = 1;
    }
  }

  int
    get_dst_regno(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, int dstno)
  {
    mrb_code **ppc = status->pc;
    mrb_code ncode = *(*ppc + 1);

    if (GET_OPCODE(ncode) == OP_MOVE && GETARG_B(ncode) == dstno) {
      if (GETARG_A(ncode) < MRBJIT_REG_VMREGMAX - MRBJIT_REG_VMREG0) {
	mrbjit_reginfo *rinfo = &coi->reginfo[dstno];

	dstno = GETARG_A(ncode);
	rinfo->regplace = (enum mrbjit_regplace)(dstno + MRBJIT_REG_VMREG0);

	mrb->compile_info.ignor_inst_cnt = 2;
      }
    }

    return dstno;
  }

  /* Generate get Array ptr
     in
     reg_tmp1 <- RArray*
     out
     reg_tmp1 <- array ptr 
     destroy reg_tmp0  */
  void
    gen_ary_ptr(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    /* read embed flag in flags */
    emit_move(mrb, coi, reg_tmp0s, reg_tmp1, 0);
    lea(reg_tmp1, ptr [reg_tmp1 + OffsetOf(struct RArray, as.embed)]);
    shr(reg_tmp0s, 11);
    and(reg_tmp0s, (uint32_t)MRB_ARY_EMBED_MASK);
    cmovz(reg_tmp1, ptr [reg_tmp1]);
  }

  /* Generate get Array ptr
     in
     reg_tmp1 <- RArray*
     out
     reg_tmp0s <- array s */
  void
    gen_ary_size(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    /* read embed flag in flags */
    emit_move(mrb, coi, reg_tmp0s, reg_tmp1, 0);
    shr(reg_tmp0s, 11);
    and(reg_tmp0s, (uint32_t)MRB_ARY_EMBED_MASK);
    dec(reg_tmp0s);
    cmovs(reg_tmp0s, ptr [reg_tmp1 + OffsetOf(struct RArray, as.heap.len)]);
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
    const cpu_word_t dstno = GETARG_A(**ppc);
    const cpu_word_t srcno = GETARG_B(**ppc);
    mrbjit_reginfo *sinfo = &coi->reginfo[GETARG_B(**ppc)];
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (srcno < MRBJIT_REG_VMREGMAX - MRBJIT_REG_VMREG0 &&
	srcno < dstno && dstno > (*status->irep)->nlocals) {
      if (sinfo->regplace == MRBJIT_REG_IMMIDATE ||
	  (sinfo->regplace >= MRBJIT_REG_VMREG0 &&
	   sinfo->regplace < MRBJIT_REG_VMREGMAX)) {
	*dinfo = *sinfo;
      }
      /* Arg pram. set */
      dinfo->regplace = (enum mrbjit_regplace)(srcno + MRBJIT_REG_VMREG0);
    }
    else {
      if (coi && coi->reginfo) {
	gen_flush_one(mrb, status, coi, srcno);
      }
      sinfo->regplace = MRBJIT_REG_MEMORY;
      sinfo->unboxedp = 0;
      *dinfo = *sinfo;

      emit_local_var_read(mrb, coi, reg_dtmp0, srcno);
      emit_local_var_write(mrb, coi, dstno, reg_dtmp0);
    }
    return code;
  }

  const void *
    ent_loadl(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_irep *irep = *status->irep;
    mrb_code **ppc = status->pc;
    const cpu_word_t dstno = GETARG_A(**ppc);
    const cpu_word_t srcoff = GETARG_Bx(**ppc) * sizeof(mrb_value);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    mrb_value val = irep->pool[GETARG_Bx(**ppc)];
    dinfo->type = (mrb_vtype)mrb_type(val);
    dinfo->klass = mrb_class(mrb, val);
    dinfo->constp = 1;
    dinfo->unboxedp = 0;

    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)irep->pool + srcoff);
    movsd(reg_dtmp0, ptr [reg_tmp0]);
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);

    return code;
  }

  const void *
    ent_loadi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrb_irep *irep = *status->irep;

    const cpu_word_t src = GETARG_sBx(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    int n = ISEQ_OFFSET_OF(*ppc + 1);

    dinfo->value = mrb_fixnum_value(src);
    dinfo->type =  MRB_TT_FIXNUM;
    dinfo->regplace = MRBJIT_REG_IMMIDATE;
    switch (GET_OPCODE(*(*ppc + 1))) {
    case OP_EQ:
    case OP_LT:
    case OP_LE:
    case OP_GT:
    case OP_GE:
    case OP_SEND:
      irep->prof_info[n] += (COMPILE_THRESHOLD + 1); /* force compile */
      break;

    default:
      gen_flush_literal(mrb, coi, GETARG_A(**ppc));
      break;
    }

    return code;
  }

  const void *
    ent_loadsym(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrb_irep *irep = *status->irep;
    const cpu_word_t dstno = GETARG_A(**ppc);
    int srcno = GETARG_Bx(**ppc);
    const uint32_t src = (uint32_t)irep->syms[srcno];
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_SYMBOL;
    dinfo->klass = mrb->symbol_class;
    dinfo->constp = 1;
    dinfo->unboxedp = 0;

    emit_load_literal(mrb, coi, reg_tmp0s, src);
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
    emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_SYMBOL);
    emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);

    return code;
  }

  const void *
    ent_loadself(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int a = GETARG_A(**ppc);
    const cpu_word_t dstno = a;
    mrbjit_reginfo *dinfo = &coi->reginfo[a];
    mrb_value self = mrb->c->stack[0];

    *dinfo = coi->reginfo[0];

    emit_local_var_read(mrb, coi, reg_dtmp0, 0); /* self */
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);
    return code;
  }

  const void *
    ent_loadt(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t dstno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (dinfo->type != MRB_TT_TRUE || 1) {
      emit_load_literal(mrb, coi, reg_tmp0s, 1);
      emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
      emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_TRUE);
      emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);
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
    const cpu_word_t dstno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    if (dinfo->type != MRB_TT_FALSE || 1) {
      emit_load_literal(mrb, coi, reg_tmp0s, 1);
      emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
      emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FALSE);
      emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);
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
    const cpu_word_t dstno = GETARG_A(**ppc);
    const int argsize = 2 * sizeof(void *);
    mrb_irep *irep = *status->irep;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;

    gen_flush_regs(mrb, *ppc, status, coi, 1);
    emit_cfunc_start(mrb, coi);
    emit_arg_push(mrb, coi, 1, (cpu_word_t)irep->syms[idpos]);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_gv_get);
    emit_cfunc_end(mrb, coi, argsize);
    emit_local_var_write_from_cfunc(mrb, coi, dstno);

    return code;
  }

  const void *
    ent_setglobal(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    const cpu_word_t srcno = GETARG_A(**ppc);
    const int argsize = 4 * sizeof(void *);
    mrb_irep *irep = *status->irep;

    gen_flush_regs(mrb, *ppc, status, coi, 1);
    emit_cfunc_start(mrb, coi);
    emit_arg_push_nan(mrb, coi, 2, reg_tmp0, srcno);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)irep->syms[idpos]);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_gv_set);
    emit_cfunc_end(mrb, coi, argsize);

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
    const cpu_word_t dstno = GETARG_A(**ppc);
    mrb_value self = mrb->c->stack[0];
    const int ivoff = mrbjit_iv_off(mrb, self, id);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;

    if (ivoff < 0) {
      return NULL;
    }

    gen_flush_regs(mrb, *status->pc, status, coi, 1);
    /* You can not change class of self in Ruby */
    if (mrb_type(self) == MRB_TT_OBJECT) {
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, 0); /* self */
      emit_move(mrb, coi, reg_tmp0, reg_tmp0,  OffsetOf(struct RObject, ivent.rootseg));
      movsd(reg_dtmp0, ptr [reg_tmp0 + ivoff * sizeof(mrb_value)]);
    }
    else {
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, 0); /* self */
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
      emit_move(mrb, coi, reg_dtmp0, reg_tmp0, ivoff * sizeof(mrb_value));
    }
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);

    return code;
  }

  const void *
    ent_getiv2(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_sym id = irep->syms[idpos];
    const cpu_word_t dstno = GETARG_A(**ppc);
    mrb_value self = mrb->c->stack[dstno];
    const int ivoff = mrbjit_iv_off(mrb, self, id);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;

    if (ivoff < 0) {
      return NULL;
    }

    gen_flush_regs(mrb, *status->pc, status, coi, 1);
    /* You can not change class of self in Ruby */
    if (mrb_type(self) == MRB_TT_OBJECT) {
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, dstno);
      emit_move(mrb, coi, reg_tmp0, reg_tmp0,  OffsetOf(struct RObject, ivent.rootseg));
      movsd(reg_dtmp0, ptr [reg_tmp0 + ivoff * sizeof(mrb_value)]);
    }
    else {
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, dstno);
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
      emit_move(mrb, coi, reg_dtmp0, reg_tmp0, ivoff * sizeof(mrb_value));
    }
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);

    return code;
  }

  const void *
    ent_setiv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t srcno = GETARG_A(**ppc);
    const int idpos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_sym id = irep->syms[idpos];
    mrb_value self = mrb->c->stack[0];
    int ivoff = mrbjit_iv_off(mrb, self, id);

    inLocalLabel();
    gen_flush_regs(mrb, *status->pc, status, coi, 1);

    if (ivoff != -1) {
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, 0); /* self */
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
      if (mrb_type(self) != MRB_TT_OBJECT) {
	test(reg_tmp0, reg_tmp0);
	jz(".nivset");
      }
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
      test(reg_tmp0, reg_tmp0);
      jnz(".fastivset");
    }

    L(".nivset");
    /* Normal instance variable set (not define iv yet) */
    emit_cfunc_start(mrb, coi);
    emit_arg_push_nan(mrb, coi, 2, reg_tmp0, srcno);
    emit_load_literal(mrb, coi, reg_tmp0s, (uint32_t)id);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_vm_iv_set);
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(cpu_word_t) + sizeof(mrb_value));

    if (ivoff != -1) {
      emit_jmp(mrb, coi, ".ivsetend");

      L(".fastivset");
      emit_local_var_read(mrb, coi, reg_dtmp0, srcno);
      emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, 0); /* self */
      emit_push(mrb, coi, reg_tmp0);
      emit_cfunc_start(mrb, coi);
      emit_arg_push(mrb, coi, 1, reg_tmp0);
      emit_arg_push(mrb, coi, 0, reg_mrb);
      emit_call(mrb, coi, (void *)mrb_write_barrier);
      emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *));
      emit_pop(mrb, coi, reg_tmp0);
      if (ivoff == -2) {
	if (mrb_type(self) == MRB_TT_OBJECT) {
	  mov(reg_tmp1, reg_tmp0);
	}
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
	ivoff =  mrb_obj_ptr(self)->iv->last_len;
	emit_add(mrb, coi, reg_tmp0,  OffsetOf(iv_tbl, last_len), 1);
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
	movsd(ptr [reg_tmp0 + ivoff * sizeof(mrb_value)], reg_dtmp0);
	emit_moves(mrb, coi, reg_tmp0,  MRB_IV_SEGMENT_SIZE * sizeof(mrb_value) + ivoff * sizeof(mrb_sym), (uint32_t)id);
      }
      else {
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
	emit_move(mrb, coi, reg_tmp0, ivoff * sizeof(mrb_value), reg_dtmp0);
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
    const cpu_word_t dstno = GETARG_A(**ppc);
    const int argsize = 2 * sizeof(void *);
    mrb_irep *irep = *status->irep;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;

    emit_cfunc_start(mrb, coi);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)irep->syms[idpos]);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_vm_cv_get);
    emit_cfunc_end(mrb, coi, argsize);
    emit_local_var_write_from_cfunc(mrb, coi, dstno);

    return code;
  }

  const void *
    ent_setcv(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int idpos = GETARG_Bx(**ppc);
    const cpu_word_t srcno = GETARG_A(**ppc);
    const int argsize = 4 * sizeof(void *);
    mrb_irep *irep = *status->irep;

    emit_cfunc_start(mrb, coi);

    emit_arg_push_nan(mrb, coi, 2, reg_tmp0, srcno);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)irep->syms[idpos]);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_vm_cv_set);
    emit_cfunc_end(mrb, coi, argsize);

    return code;
  }

  const void *
    ent_getconst(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t dstno = GETARG_A(**ppc);
    const int sympos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    const mrb_value v = mrb_vm_const_get(mrb, irep->syms[sympos]);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = (mrb_vtype)mrb_type(v);
    dinfo->klass = mrb_class(mrb, v);
    dinfo->constp = 1;
    dinfo->unboxedp = 0;

    switch (mrb_type(v)) {
    case MRB_TT_FALSE:
    case MRB_TT_TRUE:
    case MRB_TT_UNDEF:
    case MRB_TT_FIXNUM:
    case MRB_TT_SYMBOL: 
      break;
    default:
      mrb_gc_unregister(mrb, v);
      mrb_gc_register(mrb, v);
    }

    emit_load_literal(mrb, coi, reg_tmp0s, mrb_fixnum(v));
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
    emit_load_literal(mrb, coi, reg_tmp0s, v.value.ttt);
    emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);
    
    return code;
  }

  const void *
    ent_getmconst(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t dstno = GETARG_A(**ppc);
    const int sympos = GETARG_Bx(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_value *regs = mrb->c->stack;
    const mrb_value v = mrb_const_get(mrb, regs[GETARG_A(**ppc)], irep->syms[sympos]);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = (mrb_vtype)mrb_type(v);
    dinfo->klass = mrb_class(mrb, v);
    dinfo->constp = 1;

    switch (mrb_type(v)) {
    case MRB_TT_FALSE:
    case MRB_TT_TRUE:
    case MRB_TT_UNDEF:
    case MRB_TT_FIXNUM:
    case MRB_TT_SYMBOL: 
      break;
    default:
      mrb_gc_unregister(mrb, v);
      mrb_gc_register(mrb, v);
    }
    emit_load_literal(mrb, coi, reg_tmp0s, mrb_fixnum(v));
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
    emit_load_literal(mrb, coi, reg_tmp0s, v.value.ttt);
    emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);
    
    return code;
  }

  const void *
    ent_loadnil(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t dstno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_FALSE;
    dinfo->klass = mrb->nil_class;
    dinfo->constp = 1;
    dinfo->unboxedp = 0;

    emit_load_literal(mrb, coi, reg_tmp0s, 0);
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
    emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FALSE);
    emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);

    return code;
  }

#define CALL_CFUNC_BEGIN                                             \
  do {                                                               \
    emit_cfunc_start(mrb, coi);					     \
  } while (0)

#define CALL_CFUNC_STATUS(func_name, auxargs)			     \
  do {                                                               \
    lea(reg_tmp0, dword [reg_vars + VMSOffsetOf(status)]);           \
    emit_arg_push(mrb, coi, 1, reg_tmp0);			     \
\
    /* Update pc */                                                  \
    emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)(*status->pc)); \
\
    emit_arg_push(mrb, coi, 0, reg_mrb);			     \
    emit_call(mrb, coi, (void *)func_name);			     \
    emit_cfunc_end(mrb, coi, (auxargs + 2) * 4);	             \
\
    test(reg_tmp0, reg_tmp0);					     \
    jz("@f");                                                        \
    gen_exit(mrb, NULL, 0, 1, status, coi);			     \
    L("@@");                                                         \
  }while (0)

#define CALL_CFUNC_JMP(func_name, auxargs, npc)			     \
  do {                                                               \
    lea(reg_tmp0, dword [reg_vars + VMSOffsetOf(status)]);           \
    emit_arg_push(mrb, coi, 1, reg_tmp0);			     \
\
    /* Update pc */                                                  \
    emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)(*status->pc)); \
\
    emit_arg_push(mrb, coi, 0, reg_mrb);			     \
    emit_call(mrb, coi, (void *)func_name);			     \
    emit_cfunc_end(mrb, coi, (auxargs + 2) * 4);	             \
\
    emit_vm_var_read(mrb, coi, reg_tmp1, VMSOffsetOf(pc));           \
    emit_cmp(mrb, coi, reg_tmp1, (cpu_word_t)(npc)); \
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

    if (MRB_PROC_CFUNC_P(m) && m->body.func == mrbjit_attr_func[0]) {
      return mrb_symbol(m->env->stack[0]);
    }

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

    if (MRB_PROC_CFUNC_P(m) && m->body.func == mrbjit_attr_func[1]) {
      return mrb_symbol(m->env->stack[0]);
    }

    ivid = method_check(mrb, m, OP_SETIV);
    if (ivid) {
      m->body.irep->method_kind = IV_WRITER;
    }

    return ivid;
  }

  int
    gen_accessor(mrb_state *mrb, mrbjit_vmstatus *status, struct RProc *m, mrb_value recv, int a, mrbjit_code_info *coi)
  {
    mrb_sym ivid;

    if ((ivid = is_reader(mrb, m))) {
      const int ivoff = mrbjit_iv_off(mrb, recv, ivid);

      if (ivoff >= 0) {
	/* Inline IV reader */
	emit_local_var_ptr_value_read(mrb, coi, reg_tmp0, a);
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RObject, iv));
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
	emit_move(mrb, coi, reg_dtmp0, reg_tmp0, ivoff * sizeof(mrb_value));

	// regs[a] = obj;
	emit_local_var_write(mrb, coi, a, reg_dtmp0);

	return 1;
      }
    }

    if ((ivid = is_writer(mrb, m))) {
      const int ivoff = mrbjit_iv_off(mrb, recv, ivid);

      if (ivoff >= 0) {
	gen_flush_regs(mrb, *status->pc, status, coi, 1);

	/* Inline IV writer */
	emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, a);
	emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(struct RObject, iv));
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);

	// @iv = regs[a];
	emit_local_var_read(mrb, coi, reg_dtmp0, a + 1);
	emit_move(mrb, coi, reg_tmp0, ivoff * sizeof(mrb_value), reg_dtmp0);
	emit_local_var_write(mrb, coi, a, reg_dtmp0);

	// mrb_write_barrier(mrb, (struct RBasic*)obj);
	emit_cfunc_start(mrb, coi);
	emit_arg_push(mrb, coi, 1, reg_tmp1);
	emit_arg_push(mrb, coi, 0, reg_mrb);
	emit_call(mrb, coi, (void *)mrb_write_barrier);
	emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *));

	return 1;
      }
    }

    if (!MRB_PROC_CFUNC_P(m)) {
      m->body.irep->method_kind = NORMAL;
    }
    return 0;
  }

  void
    gen_setnilblock(mrb_state *mrb, int a, int n, mrbjit_code_info *coi)
  {
    //SET_NIL_VALUE(regs[a+n+1]);
    mrbjit_reginfo *binfo;
    int dstno;
    if (n == CALL_MAXARGS) {
      dstno = (a + 2);
    }
    else {
      dstno = (a + n + 1);
    }
    emit_load_literal(mrb, coi, reg_tmp0s, 0);
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
    emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_FALSE);
    emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);
    binfo = &coi->reginfo[dstno];
    binfo->unboxedp = 0;
    binfo->type = MRB_TT_FREE;
    binfo->klass = NULL;
    binfo->constp = 0;
  }

  int
    gen_send_primitive(mrb_state *mrb, struct RClass *c, mrb_sym mid, struct RProc *m, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_value prim;

    prim = mrb_obj_iv_get(mrb, (struct RObject *)c, mid);
    if (mrb_type(prim) == MRB_TT_PROC) {
      mrb_value res = ((mrbjit_prim_func_t)mrb_proc_ptr(prim)->body.func)(mrb, prim, status, coi);
      switch (mrb_type(res)) {
      case MRB_TT_PROC:
	m = mrb_proc_ptr(res);
	break;
	
      case MRB_TT_TRUE:
	if (!MRB_PROC_CFUNC_P(m)) {
	  m->body.irep->disable_jit = 1;
	}
	return 1;

      default:
	break;
      }
    }

    return 0;
  }

  const void *
    ent_send(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_code *pc = *status->pc;
    mrb_value *regs = mrb->c->stack;
    mrb_sym *syms = *status->syms;
    int i = *pc;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
    struct RProc *m;
    struct RClass *c;
    const void *code = getCurr();
    mrb_value recv;
    mrb_sym mid = syms[GETARG_B(i)];
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(i)];
    mrbjit_reginfo *sinfo;

    if (GETARG_C(i) == CALL_MAXARGS) {
      n = 1;
    }

    recv = regs[a];
    gen_class_guard(mrb, a, status, pc, coi, mrb_class(mrb, recv), 2);

    c = mrb_class(mrb, recv);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (!m) {
      return NULL;
    }

    dinfo->unboxedp = 0;
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;

    if (gen_accessor(mrb, status, m, recv, a, coi)) {
      return code;
    }
    
    if (GET_OPCODE(i) != OP_SENDB) {
      gen_setnilblock(mrb, a, n, coi);
    }
    else {
      gen_type_guard(mrb, a + n + 1, status, *status->pc, coi);
      if (mrb_type(regs[a + n + 1]) != MRB_TT_PROC) {
	gen_exit(mrb, *status->pc, 1, 1, status, coi);
      }
    }

    if (gen_send_primitive(mrb, c, mid, m, status, coi)) {
      return code;
    }

    gen_flush_regs(mrb, pc, status, coi, 1);

    if (MRB_PROC_CFUNC_P(m)) {
      //mrb_p(mrb, regs[a]);
      //puts(mrb_sym2name(mrb, mid)); // for tuning
      //printf("%x \n", irep);
      CALL_CFUNC_BEGIN;
      emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)c);
      emit_arg_push(mrb, coi, 3, reg_tmp0);
      emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)m);
      emit_arg_push(mrb, coi, 2, reg_tmp0);
      CALL_CFUNC_STATUS(mrbjit_exec_send_c, 2);
      mrb_gc_register(mrb, mrb_obj_value(m));

      /* Restore c->stack to reg_regs and update reg_context for fiber */
      emit_move(mrb, coi, reg_context, reg_mrb, OffsetOf(mrb_state, c));
      emit_move(mrb, coi, reg_regs, reg_context, OffsetOf(mrb_context, stack));
    }
    else {
      mrb_irep *sirep = m->body.irep;
      int j;
      sinfo = (sirep->jit_entry_tab + (sirep->ilen - 1))->body->reginfo;
      if (sinfo) {
	*dinfo = *sinfo;
      }
      for (j = 1; j <= n + 1; j++) {
	mrbjit_reginfo *rinfo = &coi->reginfo[a + j];

	rinfo->unboxedp = 0;
	rinfo->type = MRB_TT_FREE;
	rinfo->klass = NULL;
	rinfo->constp = 0;
      }
      gen_send_mruby(mrb, m, mid, recv, c, status, pc, coi);
    }

    return code;
  }

  const void *
    ent_tailcall(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_code *pc = *status->pc;
    mrb_value *regs = mrb->c->stack;
    mrb_sym *syms = *status->syms;
    int i = *pc;
    int j;
    int a = GETARG_A(i);
    int n = GETARG_C(i);
    mrb_code retcode = MKOP_AB(OP_RETURN, a, OP_R_NORMAL);
    struct RProc *m;
    struct RClass *c;
    const void *code = getCurr();
    mrb_value recv;
    mrb_sym mid = syms[GETARG_B(i)];
    mrbjit_reginfo *dinfo = &coi->reginfo[a];
    mrbjit_reginfo *sinfo;

    if (GETARG_C(i) == CALL_MAXARGS) {
      n = 1;
    }

    recv = regs[a];
    gen_flush_regs(mrb, pc, status, coi, 1);
    gen_class_guard(mrb, a, status, pc, coi, mrb_class(mrb, recv), 2);

    c = mrb_class(mrb, recv);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (!m) {
      return NULL;
    }

    dinfo->unboxedp = 0;
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;

    if (gen_accessor(mrb, status, m, recv, a, coi)) {
      ent_return(mrb, status, coi, retcode);
      return code;
    }
    
    gen_setnilblock(mrb, a, n, coi);

    if (gen_send_primitive(mrb, c, mid, m, status, coi)) {
      ent_return(mrb, status, coi, retcode);
      return code;
    }

    /* stack copy */

    /* move stack */
    for (j = 0; j < n + 2; j++) {
      emit_local_var_read(mrb, coi, reg_dtmp0, a + j);
      emit_local_var_write(mrb, coi, j, reg_dtmp0);
    }

    /* setup ci */
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));

    emit_moves(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, mid), (uint32_t)mid);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)c);
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_callinfo, target_class), reg_tmp0);
    emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, env), 0);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)m);
    emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, proc), reg_tmp0);
    emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp0);
    mrb_gc_register(mrb, mrb_obj_value(m));
    if (n == CALL_MAXARGS) {
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), -1);
    }
    else {
      emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, argc), n);
    }

    if (MRB_PROC_CFUNC_P(m)) {
      emit_cfunc_start(mrb, coi);

      emit_arg_push_nan(mrb, coi, 1, reg_tmp0, 0);
      emit_arg_push(mrb, coi, 0, reg_mrb);
      emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)m);
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RProc, body.func));
      call(reg_tmp0);
      emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value));

      emit_local_var_write_from_cfunc(mrb, coi, 0);

      mrb->compile_info.nest_level--;
      if (mrb->c->ci->proc->env ||
	  mrb->compile_info.nest_level != 0 ||
	  (mrb->c->ci != mrb->c->cibase &&
	   mrb->c->ci[0].proc->body.irep == mrb->c->ci[-1].proc->body.irep)) {
	ent_return(mrb, status, coi, retcode);
      }
      else {
	ent_return_inline(mrb, status, coi, retcode);
      }
    }
    else {
      mrb_irep *sirep = m->body.irep;
      int j;
      sinfo = (sirep->jit_entry_tab + (sirep->ilen - 1))->body->reginfo;
      if (sinfo) {
	*dinfo = *sinfo;
      }
      for (j = 1; j <= n + 1; j++) {
	mrbjit_reginfo *rinfo = &coi->reginfo[a + j];

	rinfo->unboxedp = 0;
	rinfo->type = MRB_TT_FREE;
	rinfo->klass = NULL;
	rinfo->constp = 0;
      }

      emit_moves(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, nregs), (uint32_t)sirep->nregs);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), (cpu_word_t)sirep);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pool), (cpu_word_t)sirep->pool);
      emit_vm_var_write(mrb, coi, VMSOffsetOf(syms), (cpu_word_t)sirep->syms);
      /* stack extend */
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)sirep->iseq);
    }

    return code;
  }

  const void *
    ent_block_guard(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    mrb_irep *irep = *status->irep;
    const void *code = getCurr();
    mrbjit_reginfo *selfinfo = &coi->reginfo[0];
    mrb_code *pc = *status->pc;
    mrb_value *regs = mrb->c->stack;
    int i;

    if (irep->block_lambda) {
      cpu_word_t room;
      int keep;
      cpu_word_t nlocal;
      
      inLocalLabel();
      emit_vm_var_read(mrb, coi, reg_tmp0, VMSOffsetOf(irep));
      emit_load_literal(mrb, coi, reg_tmp1, (cpu_word_t)irep);
      emit_cmp(mrb, coi, reg_tmp0, reg_tmp1);
      jz(".gend");

      gen_flush_regs(mrb, *status->pc, status, coi, 1);

      L(".exitlab");
      emit_vm_var_read(mrb, coi, reg_tmp0, VMSOffsetOf(irep));
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(mrb_irep, iseq));
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), reg_tmp0);
      emit_load_literal(mrb, coi, reg_tmp0, 5); /* block guard fail */
      emit_load_label(mrb, coi, reg_tmp1, ".exitlab");
      ret();

      L(".gend");
      outLocalLabel();

      if (mrb->compile_info.force_compile) {
	keep = mrb->c->ci->argc;
	room = irep->nregs;
	nlocal = irep->nlocals;
	if (keep == -1) {
	  gen_stack_extend(mrb, status, coi, (room < 3) ? 3 : room, 3,
			   nlocal < 1 ? 3 : nlocal + 2);
	}
	else {
	  gen_stack_extend(mrb, status, coi, (room < 3) ? 3 : room, keep + 2, nlocal);
	}
      }
    }

    if (GET_OPCODE(*pc) != OP_ENTER ||
	(MRB_ASPEC_OPT(GETARG_Ax(*pc)) == 0 &&
	 MRB_ASPEC_REST(GETARG_Ax(*pc)) == 0)) {
      gen_class_guard(mrb, 0, status, pc, coi, mrb_class(mrb, regs[0]), 2);
      selfinfo->type = (mrb_vtype)mrb_type(regs[0]);
      selfinfo->klass = mrb_class(mrb, regs[0]);
      selfinfo->constp = 1;
      if (mrb->c->ci->argc != -1) {
	int i = mrb->c->ci->argc + 1;
	gen_class_guard(mrb, i, status, pc, coi, mrb_class(mrb, regs[i]), 2);
      }
    }
#if 0
    if (GET_OPCODE(*pc) != OP_ENTER ||
	(MRB_ASPEC_OPT(GETARG_Ax(*pc)) == 0 &&
	 MRB_ASPEC_REST(GETARG_Ax(*pc)) == 0)) {
      /* + 1 means block */
      if (mrb->c->ci->argc != -1) {
	for (i = 0; i <= mrb->c->ci->argc + 1; i++) {
	  gen_class_guard(mrb, i, status, pc, coi, mrb_class(mrb, regs[i]), 2);
	}
	//gen_class_guard(mrb, 0, status, pc, coi, mrb_class(mrb, regs[i]), 2);
      }
    }
#endif

    if (code == getCurr()) {
      return NULL;
    }

    return code;
  }

  const void *
    ent_enter(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code *pc = *status->pc;
    mrb_code ins = *pc;
    /* Ax             arg setup according to flags (23=5:5:1:5:5:1:1) */
    /* number of optional arguments times OP_JMP should follow */
    mrb_aspec ax = GETARG_Ax(ins);
    int m1 = MRB_ASPEC_REQ(ax);
    int o  = MRB_ASPEC_OPT(ax);
    int r  = MRB_ASPEC_REST(ax);
    int m2 = MRB_ASPEC_POST(ax);
    /* unused
       int k  = (ax>>2)&0x1f;
       int kd = (ax>>1)&0x1;
       int b  = (ax>>0)& 0x1;
    */

    if (mrb->c->ci->argc < 0 || (ax & ((1 << 18) - 1)) != 0 ||
	m1 > mrb->c->ci->argc) {
      int argc = mrb->c->ci->argc;
      mrb_code *npc = *status->pc + 1;

      if (argc-m1-o-m2 == 1 && argc >= 0) {
	mrb_irep *irep = *status->irep;
	mrb_irep *nirep = (mrb_irep *) mrb;

	if (irep->pool) {
	  nirep = (mrb_irep *)((uintptr_t)mrb + mrb_fixnum(irep->pool[0]));
	}
	if (nirep != (mrb_irep *)mrb) {
	  npc = nirep->iseq + 1;
	}
      }

      if (o > 0 && argc >= m1 + m2) {
	npc += argc - m1 - m2;
      }

      CALL_CFUNC_BEGIN;
      CALL_CFUNC_JMP(mrbjit_exec_enter, 0, npc);
      if (r) {
	mrbjit_reginfo *ainfo = &coi->reginfo[m1 + o + 1];
	
	ainfo->type = MRB_TT_ARRAY;
	ainfo->klass = mrb->array_class;
	ainfo->constp = 0;
	ainfo->unboxedp = 0;
      }
    }
    else {
      int len = m1 + o + r + m2;
      mrb_irep *irep = *status->irep;

      if ((int)(irep->nlocals-len-2) > 0) {
	gen_stack_clear(mrb, status, coi, len + 2, irep->nlocals);
      }
    }

    return code;
  }

  const void *
    ent_return(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_code i)
  {
    const void *code = getCurr();
    struct mrb_context *c = mrb->c;
    mrb_code *pc = *status->pc;
    int can_use_fast = (c->ci != c->cibase &&
			GETARG_B(i) == OP_R_NORMAL &&
			(c->ci->env == 0 || 
			 c->ci->proc->body.irep->shared_lambda == 1));
    int can_inline = (can_use_fast &&
		      (c->ci[-1].epos == c->eidx) && (c->ci[-1].acc >= 0));

    mrbjit_reginfo *rinfo = &coi->reginfo[GETARG_A(i)];
    mrbjit_reginfo *dinfo = &coi->reginfo[0];

#if 0
    mrb_value sclass = mrb_obj_value(mrb, mrb_obj_class(mrb, regs[0]));
    printf("%s#%s -> ", 
	   RSTRING_PTR(mrb_funcall(mrb, sclass, "inspect", 0)), 
	   mrb_sym2name(mrb, mrb->c->ci->mid));
    disp_type(mrb, rinfo);
#endif

    gen_flush_regs(mrb, pc, status, coi, 1);
    *dinfo = *rinfo;

    inLocalLabel();

    /* Update pc */
    emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)(*status->pc));

    if (can_inline) {
      /* Check must copy env */
      emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
      /*      emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(mrb_callinfo, env));
      test(reg_tmp0, reg_tmp0);
      jnz(".reg_vm");*/

      /* Check exception happened? */
      emit_move(mrb, coi, reg_tmp0, reg_mrb, OffsetOf(mrb_state, exc));
      test(reg_tmp0, reg_tmp0);
      jnz(".reg_vm");

      /* Inline else part of mrbjit_exec_return_fast (but not ensure call) */

      /* Save return value */
      emit_local_var_read(mrb, coi, reg_dtmp0, GETARG_A(i));
      /* Store return value (bottom of stack always return space) */
      emit_local_var_write(mrb, coi, 0, reg_dtmp0);

      /* Restore Regs */
      emit_move(mrb, coi, reg_regs, reg_tmp1, OffsetOf(mrb_callinfo, stackent));

      /* Restore c->stack */
      emit_move(mrb, coi, reg_context, OffsetOf(mrb_context, stack), reg_regs);

      /* Restore PC */
      emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(mrb_callinfo, pc));
      emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), reg_tmp0);

      /* pop ci */
      emit_sub(mrb, coi, reg_tmp1, (cpu_word_t)sizeof(mrb_callinfo));
      emit_move(mrb, coi, reg_context, OffsetOf(mrb_context, ci), reg_tmp1);

      /* restore proc */
      emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(mrb_callinfo, proc));
      emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp0);

      /* restore irep */
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RProc, body.irep));
      emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), reg_tmp0);
    }
    else {
      emit_cfunc_start(mrb, coi);

      lea(reg_tmp0, dword [reg_vars + VMSOffsetOf(status)]);
      emit_arg_push(mrb, coi, 1, reg_tmp0);
      emit_arg_push(mrb, coi, 0, reg_mrb);
      if (can_use_fast && 0) {
	emit_call(mrb, coi, (void *)mrbjit_exec_return_fast);
      }
      else {
	emit_call(mrb, coi, (void *)mrbjit_exec_return);
      }
      emit_cfunc_end(mrb, coi, 2 * 4);

      test(reg_tmp0, reg_tmp0);
      jz("@f");
      gen_exit(mrb, NULL, 0, 0, status, coi);
      L("@@");

      /* update reg_context for Fiber auto resume */
      emit_move(mrb, coi, reg_context, reg_mrb, OffsetOf(mrb_state, c));
      emit_move(mrb, coi, reg_regs, reg_context, OffsetOf(mrb_context, stack));
      emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
    }

    if ((*status->irep)->jit_inlinep == 0) {
      /* Set return address from callinfo */
      emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(mrb_callinfo, jit_entry));
      test(reg_tmp0, reg_tmp0);
      jnz("@f");

      L(".reg_vm");
      gen_exit(mrb, NULL, 1, 1, status, coi);

      L("@@");
      emit_jmp(mrb, coi, reg_tmp0);
    }
    else {
      emit_jmp(mrb, coi, "@f");

      L(".reg_vm");
      gen_exit(mrb, NULL, 1, 1, status, coi);

      L("@@");
    }

    outLocalLabel();

    return code;
  }

  const void *
    ent_return_inline(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_code i)
  {
    const void *code = getCurr();
    mrb_code *pc = *status->pc;

#if 0
    mrb_value *regs = mrb->c->stack;
    mrb_code i = *pc;
    mrbjit_reginfo *rinfo = &coi->reginfo[GETARG_A(i)];
    mrb_value sclass = mrb_obj_value(mrb, mrb_obj_class(mrb, regs[0]));
    printf("%s#%s -> ", 
	   RSTRING_PTR(mrb_funcall(mrb, sclass, "inspect", 0)), 
	   mrb_sym2name(mrb, mrb->c->ci->mid));
    disp_type(mrb, rinfo);
#endif

    gen_flush_regs(mrb, pc, status, coi, 1);

    inLocalLabel();

    /* Update pc */
    emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), (cpu_word_t)(*status->pc));

    /* Set return address from callinfo */
    
    /* Check exception happened? */
    emit_move(mrb, coi, reg_tmp0, reg_mrb, OffsetOf(mrb_state, exc));
    test(reg_tmp0, reg_tmp0);
    jz(".skip_reg_vm");

    gen_exit(mrb, *status->pc, 1, 0, status, coi);

    L(".skip_reg_vm");

    /* Inline else part of mrbjit_exec_return_fast (but not ensure call) */

    /* Save return value */
    emit_local_var_read(mrb, coi, reg_dtmp0, GETARG_A(i));

    /* Store return value (bottom of stack always return space) */
    emit_local_var_write(mrb, coi, 0, reg_dtmp0);

    /* Restore Regs */
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
    emit_move(mrb, coi, reg_regs, reg_tmp1, OffsetOf(mrb_callinfo, stackent));

    /* Restore c->stack */
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_context, stack), reg_regs);

    /* Restore PC */
    emit_move(mrb, coi, reg_tmp0, reg_tmp1, OffsetOf(mrb_callinfo, pc));
    emit_vm_var_write(mrb, coi, VMSOffsetOf(pc), reg_tmp0);

    /* pop ci */
    emit_sub(mrb, coi, reg_tmp1, (cpu_word_t)sizeof(mrb_callinfo));
    emit_move(mrb, coi, reg_context, OffsetOf(mrb_context, ci), reg_tmp1);

    /* restore proc */
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(mrb_callinfo, proc));
    emit_vm_var_write(mrb, coi, VMSOffsetOf(proc), reg_tmp1);

    /* restore irep */
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(struct RProc, body.irep));
    emit_vm_var_write(mrb, coi, VMSOffsetOf(irep), reg_tmp1);

    outLocalLabel();

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
      if (mrb_nil_p(regs[m1 + r + m2 + 1])) {
	return NULL;
      }
      emit_local_var_read(mrb, coi, reg_dtmp0, m1 + r + m2 + 1);
      emit_local_var_write(mrb, coi, a, reg_dtmp0);
    }
    else {
      int i;
      int up = lv - 1;
      struct REnv *e = mrb->c->ci->proc->env;

      while (up--) {
	if (!e) return NULL;
	e = (struct REnv*)e->c;
      }

      if (!e) {
	return NULL;
      }
      emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(mrb_context, ci));
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(mrb_callinfo, proc));
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RProc, env));
      for (i = 0; i < lv - 1; i++) {
	emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct REnv, c));
      }
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct REnv, stack));
      emit_move(mrb, coi, reg_dtmp0, reg_tmp0,  (m1 + r + m2 + 1) * sizeof(mrb_value));
      emit_local_var_write(mrb, coi, a, reg_dtmp0);
    }

    return code;
  }

  const void *
    ent_epush(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code *pc = *status->pc;
    /* Bx     ensure_push(SEQ[Bx]) */

    /* execute by RITE VM */
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)status->optable[GET_OPCODE(*pc)]);
    gen_exit(mrb, pc, 0, 0, status, coi);

    return code;
  }

#define OVERFLOW_CHECK_GEN(AINST)                                       \
    jno("@f", T_NEAR);							\
    emit_local_var_int_value_read(mrb, coi, reg_dtmp0, reg0pos);	\
    emit_local_var_int_value_read(mrb, coi, reg_dtmp1, reg1pos);	\
    AINST(mrb, coi, reg_dtmp0, reg_dtmp1);				\
    dstno = get_dst_regno(mrb, status, coi, reg0pos);			\
    dinfo = &coi->reginfo[dstno];					\
    dinfo->unboxedp = 0;                                              \
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);			\
    gen_exit(mrb, *status->pc + 1, 1, 2, status, coi);			\
    L("@@");                                                            \


#define ARTH_GEN(AINST)                                                 \
  do {                                                                  \
    int reg0pos = GETARG_A(**ppc);                                      \
    int reg1pos = reg0pos + 1;                                          \
    enum mrb_vtype r0type = (enum mrb_vtype) mrb_type(regs[reg0pos]);   \
    enum mrb_vtype r1type = (enum mrb_vtype) mrb_type(regs[reg1pos]);   \
    int dstno;								\
    mrbjit_reginfo *dinfo;						\
\
    if (r0type == MRB_TT_FIXNUM && r1type == MRB_TT_FIXNUM) {           \
\
      gen_type_guard(mrb, reg0pos, status, *ppc, coi);			\
      gen_type_guard(mrb, reg1pos, status, *ppc, coi);			\
\
      emit_local_var_value_read(mrb, coi, reg_tmp0s, reg0pos);		\
      emit_local_var_value_read(mrb, coi, reg_tmp1s, reg1pos);		\
      AINST(mrb, coi, reg_tmp0s, reg_tmp1s);				\
      OVERFLOW_CHECK_GEN(AINST);                                        \
      if (dinfo->regplace != MRBJIT_REG_MEMORY ||			\
          dinfo->type != MRB_TT_FIXNUM || 1) {				\
        emit_load_literal(mrb, coi, reg_tmp1s, 0xfff00000 | MRB_TT_FIXNUM);\
        emit_local_var_type_write(mrb, coi, dstno, reg_tmp1s);          \
      }                                                                 \
      emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);		\
      if ((*status->irep)->may_overflow == 0) {                         \
	dinfo->regplace = MRBJIT_REG_MEMORY;				\
        dinfo->type = MRB_TT_FIXNUM;					\
        dinfo->klass = mrb->fixnum_class; 				\
      }                                                                 \
    }                                                                   \
    else if ((r0type == MRB_TT_FLOAT || r0type == MRB_TT_FIXNUM) &&     \
             (r1type == MRB_TT_FLOAT || r1type == MRB_TT_FIXNUM)) {	\
\
      gen_type_guard(mrb, reg0pos, status, *ppc, coi);			\
      gen_type_guard(mrb, reg1pos, status, *ppc, coi);			\
\
      if (r0type == MRB_TT_FIXNUM) {                                    \
        emit_local_var_int_value_read(mrb, coi, reg_dtmp0, reg0pos);    \
      }                                                                 \
      else {                                                            \
	emit_local_var_read(mrb, coi, reg_dtmp0, reg0pos);		\
      }                                                                 \
\
      if (r1type == MRB_TT_FIXNUM) {                                    \
        emit_local_var_int_value_read(mrb, coi, reg_dtmp1, reg1pos);    \
      }                                                                 \
      else {                                                            \
	emit_local_var_read(mrb, coi, reg_dtmp1, reg1pos);		\
      }                                                                 \
\
      AINST(mrb, coi, reg_dtmp0, reg_dtmp1);				\
\
      dstno = get_dst_regno(mrb, status, coi, reg0pos);			\
      dinfo = &coi->reginfo[dstno];					\
      dinfo->unboxedp = 0;                                              \
      emit_local_var_write(mrb, coi, dstno, reg_dtmp0);			\
      dinfo->regplace = MRBJIT_REG_MEMORY;				\
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
    ARTH_GEN(emit_add);
    return code;
  }

  const void *
    ent_sub(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(emit_sub);
    return code;
  }

  const void *
    ent_mul(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_GEN(emit_mul);
    return code;
  }

  const void *
    ent_div(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int reg0pos = GETARG_A(**ppc);
    int reg1pos = reg0pos + 1;
    enum mrb_vtype r0type = (enum mrb_vtype) mrb_type(regs[reg0pos]);
    enum mrb_vtype r1type = (enum mrb_vtype) mrb_type(regs[reg1pos]);
    mrbjit_reginfo *dinfo = &coi->reginfo[reg0pos];
    if ((r0type != MRB_TT_FIXNUM && r0type != MRB_TT_FLOAT) ||
	(r1type != MRB_TT_FIXNUM && r1type != MRB_TT_FLOAT)) {
      return NULL;
    }
    gen_type_guard(mrb, reg0pos, status, *ppc, coi);
    gen_type_guard(mrb, reg1pos, status, *ppc, coi);

    if (r0type == MRB_TT_FIXNUM) {
      emit_local_var_int_value_read(mrb, coi, reg_dtmp0, reg0pos);
    }
    else {
      emit_local_var_read(mrb, coi, reg_dtmp0, reg0pos);
    }

    if (r1type == MRB_TT_FIXNUM) {
      emit_local_var_int_value_read(mrb, coi, reg_dtmp1, reg1pos);
    }
    else {
      emit_local_var_read(mrb, coi, reg_dtmp1, reg1pos);
    }

    emit_div(mrb, coi, reg_dtmp0, reg_dtmp1);
    emit_local_var_write(mrb, coi, reg0pos, reg_dtmp0);

    /* Div returns Float always */
    /* see http://qiita.com/monamour555/items/bcef9b41a5cc4670675a */
    dinfo->type = MRB_TT_FLOAT;
    dinfo->klass = mrb->float_class;

    return code;
  }

#define OVERFLOW_CHECK_I_GEN(AINST)                                     \
    jno("@f", T_NEAR);							\
    emit_local_var_int_value_read(mrb, coi, reg_dtmp0, regno);	        \
    emit_load_literal(mrb, coi, reg_tmp0, y);				\
    cvtsi2sd(reg_dtmp1, reg_tmp0);                                      \
    AINST(mrb, coi, reg_dtmp0, reg_dtmp1);				\
    dstno = get_dst_regno(mrb, status, coi, regno);			\
    dinfo = &coi->reginfo[dstno];					\
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);			\
    gen_exit(mrb, *status->pc + 1, 1, 2, status, coi);			\
    L("@@");                                                            \

#define ARTH_I_GEN(AINST)                                               \
  do {                                                                  \
    const cpu_word_t y = GETARG_C(**ppc);                               \
    int regno = GETARG_A(**ppc);                                        \
    enum mrb_vtype atype = (enum mrb_vtype) mrb_type(regs[regno]);      \
    int dstno;								\
    mrbjit_reginfo *dinfo;						\
\
    gen_type_guard(mrb, regno, status, *ppc, coi);			\
\
    if (atype == MRB_TT_FIXNUM) {                                       \
      emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);		\
      AINST(mrb, coi, reg_tmp0s, y);				        \
      OVERFLOW_CHECK_I_GEN(AINST);                                      \
      if (dinfo->regplace != MRBJIT_REG_MEMORY ||			\
          dinfo->type != MRB_TT_FIXNUM || 1) {				\
        emit_load_literal(mrb, coi, reg_tmp1s, 0xfff00000 | MRB_TT_FIXNUM);\
        emit_local_var_type_write(mrb, coi, dstno, reg_tmp1s);          \
      }                                                                 \
      emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);		\
      if ((*status->irep)->may_overflow == 0) {                         \
	dinfo->regplace = MRBJIT_REG_MEMORY;				\
        dinfo->type = MRB_TT_FIXNUM;					\
        dinfo->klass = mrb->fixnum_class; 				\
      }                                                                 \
    }                                                                   \
    else if (atype == MRB_TT_FLOAT) {					\
      emit_local_var_read(mrb, coi, reg_dtmp0, regno);			\
      emit_load_literal(mrb, coi, reg_tmp0, y);                         \
      cvtsi2sd(reg_dtmp1, reg_tmp0);                                    \
      AINST(mrb, coi, reg_dtmp0, reg_dtmp1);				\
      dstno = get_dst_regno(mrb, status, coi, regno);			\
      dinfo = &coi->reginfo[dstno];					\
      emit_local_var_write(mrb, coi, dstno, reg_dtmp0);			\
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
    ARTH_I_GEN(emit_add);
    return code;
  }

  const void *
    ent_subi(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    ARTH_I_GEN(emit_sub);
    return code;
  }

#define TYPES2(a,b) ((((uint16_t)(a))<<8)|(((uint16_t)(b))&0xff))
#define OP_CMP_BODY(op,v1,v2) (v1(regs[regno]) op v2(regs[regno+1]))

#define OP_CMP(op) ({				\
      int result = -1;						\
      /* need to check if - is overridden */			\
      switch (TYPES2(mrb_type(regs[regno]),mrb_type(regs[regno+1]))) {	\
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):			\
	result = OP_CMP_BODY(op,mrb_fixnum,mrb_fixnum);		\
	break;							\
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):			\
	result = OP_CMP_BODY(op,mrb_fixnum,mrb_float);		\
	break;							\
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):			\
	result = OP_CMP_BODY(op,mrb_float,mrb_fixnum);		\
	break;							\
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):			\
	result = OP_CMP_BODY(op,mrb_float,mrb_float);		\
	break;							\
      default:							\
	break;							\
      }								\
      result;							\
    })

#define COMP_GEN_II(CMPINST)                                         \
do {                                                                 \
    emit_local_var_value_read(mrb, coi, reg_tmp0s, regno);	     \
    emit_local_var_cmp(mrb, coi, reg_tmp0s, regno + 1);		     \
    CMPINST;     						     \
} while(0)

#define COMP_GEN_IF(CMPINST)                                         \
do {                                                                 \
    emit_local_var_int_value_read(mrb, coi, reg_dtmp0, regno);	     \
    emit_local_var_cmp(mrb, coi, reg_dtmp0, regno + 1);		     \
    CMPINST;    						     \
} while(0)

#define COMP_GEN_FI(CMPINST)                                         \
do {                                                                 \
    emit_local_var_read(mrb, coi, reg_dtmp0, regno);		     \
    emit_local_var_int_value_read(mrb, coi, reg_dtmp1, regno + 1);   \
    comisd(reg_dtmp0, reg_dtmp1);     			             \
    CMPINST;     						     \
} while(0)

#define COMP_GEN_FF(CMPINST)                                         \
do {                                                                 \
    emit_local_var_read(mrb, coi, reg_dtmp0, regno);		     \
    emit_local_var_cmp(mrb, coi, reg_dtmp0, regno + 1);		     \
    CMPINST;    						     \
} while(0)
    
#define COMP_GEN_SS(CMPINST)                                         \
do {                                                                 \
    emit_cfunc_start(mrb, coi);					     \
    emit_arg_push_nan(mrb, coi, 2, reg_vars, regno + 1);             \
    emit_arg_push_nan(mrb, coi, 1, reg_vars, regno);                 \
    emit_arg_push(mrb, coi, 0, reg_mrb);			     \
    emit_call(mrb, coi, (void *)mrb_str_cmp);			     \
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value) * 2); \
    test(reg_tmp0, reg_tmp0);                                                  \
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
    else if (mrb_type(regs[regno]) == MRB_TT_STRING &&		     \
             mrb_type(regs[regno + 1]) == MRB_TT_STRING) {           \
          gen_type_guard(mrb, regno, status, *ppc, coi);	     \
          gen_type_guard(mrb, regno + 1, status, *ppc, coi);	     \
                                                                     \
          COMP_GEN_SS(CMPINSTI);                                     \
    }                                                                \
    else {                                                           \
      return ent_send(mrb, status, coi);			     \
    }                                                                \
 } while(0)

#define COMP_BOOL_SET                                                \
do {								     \
    emit_bool_boxing(mrb, coi, reg_tmp0s);                           \
    emit_local_var_type_write(mrb, coi, regno, reg_tmp0s);	     \
    emit_load_literal(mrb, coi, reg_tmp0s, 1);                       \
    emit_local_var_value_write(mrb, coi, regno, reg_tmp0s);	     \
  } while(0)

#define COMP_GEN(CMPINSTI, CMPINSTF)			             \
do {                                                                 \
    int regno = GETARG_A(**ppc);	                             \
                                                                     \
    COMP_GEN_CMP(CMPINSTI, CMPINSTF);                                \
    COMP_BOOL_SET;                                                   \
} while(0)

#define COMP_GEN_JMP(CMPINSTI, CMPINSTF, COP)                 	     \
do {                                                                 \
  int regno = GETARG_A(**ppc);					     \
  int cond;							     \
  cond = OP_CMP(COP);						     \
    								     \
  COMP_GEN_CMP(CMPINSTI, CMPINSTF);				     \
  if (cond < 0) {                                                    \
    COMP_BOOL_SET;						     \
    return code;						     \
  } 								     \
  switch (GET_OPCODE(*(*ppc + 1))) {				     \
  case OP_JMPIF:						     \
    ent_jmpif_inline(mrb, status, coi, cond);			     \
    break;							     \
                                                                     \
  case OP_JMPNOT:						     \
    ent_jmpnot_inline(mrb, status, coi, cond);			     \
    break;							     \
                                                                     \
  default:							     \
    COMP_BOOL_SET;						     \
    return code;						     \
  }								     \
 } while(0)

  const void *
    ent_eq(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int regno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[regno];

#if 1
    COMP_GEN_JMP(setz(al), setz(al), ==);
    return code;
#endif
      
    COMP_GEN(setz(al), setz(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
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
    COMP_GEN_JMP(setl(al), setb(al), <);
    return code;
#endif

    COMP_GEN(setl(al), setb(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
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
    COMP_GEN_JMP(setle(al), setbe(al), <=);
    return code;
#endif

    COMP_GEN(setle(al), setbe(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
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
    COMP_GEN_JMP(setg(al), seta(al), >);
    return code;
#endif

    COMP_GEN(setg(al), seta(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
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
    COMP_GEN_JMP(setge(al), setae(al), >=);
    return code;
#endif

    COMP_GEN(setge(al), setae(al));

    dinfo->type = MRB_TT_TRUE;
    dinfo->klass = mrb->true_class;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;
    return code;
  }

  const void *
    ent_array(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstno = GETARG_A(**ppc);
    int srcoff = GETARG_B(**ppc) * sizeof(mrb_value);
    int siz = GETARG_C(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    gen_flush_regs(mrb, *ppc, status, coi, 1);
    emit_cfunc_start(mrb, coi);

    lea(reg_tmp0, ptr [reg_regs + srcoff]);
    emit_arg_push(mrb, coi, 2, reg_tmp0);
    emit_load_literal(mrb, coi, reg_tmp0, siz);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_ary_new_from_values);
    
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(int) + sizeof(mrb_value *));

    emit_local_var_write_from_cfunc(mrb, coi, dstno);
    
    emit_move(mrb, coi, reg_tmp0s, reg_vars, VMSOffsetOf(ai));
    emit_move(mrb, coi, reg_mrb, OffsetOf(mrb_state, gc.arena_idx), reg_tmp0s);

    dinfo->type = MRB_TT_ARRAY;
    dinfo->klass = mrb->array_class;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    return code;
  }

  const void *
    ent_arycat(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstno = GETARG_A(**ppc);
    int srcno = GETARG_B(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];

    emit_cfunc_start(mrb, coi);

    emit_arg_push_nan(mrb, coi, 1, reg_tmp0, srcno);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_ary_splat);

    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value));

    emit_cfunc_start(mrb, coi);

    /* rc of splat */
    emit_arg_push_from_cfunc(mrb, coi, 2);

    /* arg1 reg */
    emit_arg_push_nan(mrb, coi, 1, reg_tmp0, dstno);

    /* mrb */
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_ary_concat);
    
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value) * 2);

    emit_move(mrb, coi, reg_tmp0s, reg_vars, VMSOffsetOf(ai));
    emit_move(mrb, coi, reg_mrb, OffsetOf(mrb_state, gc.arena_idx), reg_tmp0s);

    dinfo->type = MRB_TT_ARRAY;
    dinfo->klass = mrb->array_class;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;
    dinfo->regplace = MRBJIT_REG_MEMORY;
    return code;
  }

  const void *
    ent_aref(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstno = GETARG_A(**ppc);
    int srcno = GETARG_B(**ppc);
    int idx = GETARG_C(**ppc);

    if (!mrb_array_p(regs[srcno]) ||
	idx >= RARRAY_LEN(regs[srcno])) {
      return NULL;
    }

    emit_local_var_ptr_value_read(mrb, coi, reg_tmp1, srcno);
    gen_ary_size(mrb, status, coi);
    emit_cmp(mrb, coi, reg_tmp0s, idx);
    jle("@f");
    gen_ary_ptr(mrb, status, coi);
    movsd(reg_dtmp0, ptr [reg_tmp1 + idx * sizeof(mrb_value)]);
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);

    L("@@");

    return code;
  }

  const void *
    ent_getupvar(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t uppos = GETARG_C(**ppc);
    const cpu_word_t idxpos = GETARG_B(**ppc);
    const cpu_word_t dstno = GETARG_A(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    cpu_word_t i;
    dinfo->type = MRB_TT_FREE;
    dinfo->klass = NULL;
    dinfo->constp = 0;
    dinfo->unboxedp = 0;

    emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(mrb_context, ci));
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(mrb_callinfo, proc));
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RProc, env));
    for (i = 0; i < uppos; i++) {
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct REnv, c));
    }
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct REnv, stack));

    emit_move(mrb, coi, reg_dtmp0, reg_tmp0, idxpos * sizeof(mrb_value));
    emit_local_var_write(mrb, coi, dstno, reg_dtmp0);

    return code;
  }

  const void *
    ent_setupvar(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const cpu_word_t uppos = GETARG_C(**ppc);
    const cpu_word_t idxpos = GETARG_B(**ppc);
    const cpu_word_t valno = GETARG_A(**ppc);
    cpu_word_t i;

    emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(mrb_context, ci));
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(mrb_callinfo, proc));
    emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct RProc, env));
    for (i = 0; i < uppos; i++) {
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, OffsetOf(struct REnv, c));
    }
    emit_move(mrb, coi, reg_tmp1, reg_tmp0, OffsetOf(struct REnv, stack));
    emit_cfunc_start(mrb, coi);
    emit_arg_push(mrb, coi, 1, reg_tmp0);

    emit_local_var_read(mrb, coi, reg_dtmp0, valno);
    emit_move(mrb, coi, reg_tmp1, idxpos * sizeof(mrb_value), reg_dtmp0);

    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_write_barrier);
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(struct RBasic *));

    return code;
  }

  const void *
    ent_jmp(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
  {
    const void *code = getCurr();
    gen_flush_regs(mrb, *status->pc, status, coi, 1);
    return code;
  }

  void
    ent_jmpif_inline(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_int cond)
  {
    mrb_code **ppc = status->pc;

    mrb->compile_info.ignor_inst_cnt = 2;

    test(al, al);
    if (cond) {
      jnz("@f");
      gen_exit(mrb, *ppc + 2, 3, 0, status, coi);
      L("@@");
    }
    else {
      jz("@f");
      gen_exit(mrb, *ppc + GETARG_sBx(*(*ppc + 1)) + 1, 3, 0, status, coi);
      L("@@");
    }
  }

  const void *
    ent_jmpif(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int cond = GETARG_A(**ppc);

    emit_local_var_type_read(mrb, coi, reg_tmp0s, cond);
    if (mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 1, cond, *ppc + 1, status, coi);
    }
    else {
      gen_bool_guard(mrb, 0, cond, *ppc + GETARG_sBx(**ppc), status, coi);
    }

    return code;
  }

  void
    ent_jmpnot_inline(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_int cond)
  {
    mrb_code **ppc = status->pc;

    mrb->compile_info.ignor_inst_cnt = 2;

    test(al, al);
    if (!cond) {
      jz("@f");
      gen_exit(mrb, *ppc + 2, 3, 0, status, coi);
      L("@@");
    }
    else {
      jnz("@f");
      gen_exit(mrb, *ppc + GETARG_sBx(*(*ppc + 1)) + 1, 3, 0, status, coi);
      L("@@");
    }
  }

  const void *
    ent_jmpnot(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    const int cond = GETARG_A(**ppc);

    emit_local_var_type_read(mrb, coi, reg_tmp0s, cond);
    if (!mrb_test(regs[cond])) {
      gen_bool_guard(mrb, 0, cond, *ppc + 1, status, coi);
    }
    else {
      gen_bool_guard(mrb, 1, cond, *ppc + GETARG_sBx(**ppc), status, coi);
    }

    return code;
  }

  const void *
    ent_onerr(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;

    emit_move(mrb, coi, reg_tmp1s, reg_context, OffsetOf(struct mrb_context, rsize));
    emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(struct mrb_context, ci));
    emit_move(mrb, coi, reg_tmp0s, reg_tmp0, OffsetOf(mrb_callinfo, ridx));
    emit_cmp(mrb, coi, reg_tmp1s, reg_tmp0s);
    ja("@f", T_NEAR);
    gen_exit(mrb, NULL, 0, 1, status, coi);
    L("@@");
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(struct mrb_context, rescue));
    emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(struct mrb_context, ci));
    emit_move(mrb, coi, reg_tmp0s, reg_tmp0, OffsetOf(mrb_callinfo, ridx));
    lea(reg_tmp1, ptr [reg_tmp1 + reg_tmp0 * sizeof(mrb_code *)]);
    emit_move(mrb, coi, reg_tmp1, 0, (cpu_word_t)(*ppc + GETARG_sBx(**ppc)));
    emit_add(mrb, coi, reg_tmp0s, 1);
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(struct mrb_context, ci));
    emit_move(mrb, coi, reg_tmp1, OffsetOf(mrb_callinfo, ridx), reg_tmp0s);

    return code;
  }

  const void *
    ent_poperr(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int a = GETARG_A(**ppc);

    emit_move(mrb, coi, reg_tmp0, reg_context, OffsetOf(struct mrb_context, ci));
    emit_move(mrb, coi, reg_tmp1s, reg_tmp0, OffsetOf(mrb_callinfo, ridx));
    emit_sub(mrb, coi, reg_tmp1s, a);
    emit_move(mrb, coi,  reg_tmp0, OffsetOf(mrb_callinfo, ridx), reg_tmp1s);

    return code;
  }

  const void *
    ent_lambda(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs)
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int i;
    const int lno = GETARG_b(**ppc);
    const int dstno = GETARG_A(**ppc);
    const int flags = GETARG_C(**ppc);
    mrb_irep *irep = *status->irep;
    mrb_irep *mirep =irep->reps[lno];
    struct mrb_context *c = mrb->c;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_PROC;
    dinfo->klass = mrb->proc_class;
    dinfo->constp = 1;
    dinfo->unboxedp = 0;

    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)mirep);
    emit_move(mrb, coi, reg_tmp1, reg_context, OffsetOf(mrb_context, ci));
    emit_move(mrb, coi, reg_tmp1, reg_tmp1, OffsetOf(mrb_callinfo, proc));
    emit_move(mrb, coi, reg_tmp0, OffsetOf(mrb_irep, outer), reg_tmp1);
	  
    if (mirep->simple_lambda == 1 && c->proc_pool) {
      for (i = -1; c->proc_pool[i].proc.tt == MRB_TT_PROC; i--) {
	if (c->proc_pool[i].proc.body.irep == mirep) {
	  struct RProc *nproc = &c->proc_pool[i].proc;
	  emit_load_literal(mrb, coi, reg_tmp0s, (cpu_word_t)nproc - (cpu_word_t)mrb);
	  emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);
	  emit_load_literal(mrb, coi, reg_tmp0s, 0xfff00000 | MRB_TT_PROC);
	  emit_local_var_type_write(mrb, coi, dstno, reg_tmp0s);
	  /* mov(reg_tmp1, (cpu_word_t)nproc->env);
	     mov(dword [reg_tmp1 + OffsetOf(struct REnv, stack)], reg_regs);
	     mov(reg_tmp0, dword [reg_mrb + OffsetOf(mrb_state, c)]);
	     mov(reg_tmp0, dword [reg_tmp0 + OffsetOf(mrb_context, ci)]);
	     mov(reg_tmp0, dword [reg_tmp0 + OffsetOf(mrb_callinfo, proc)]);
	     mov(reg_tmp0, dword [reg_tmp0 + OffsetOf(struct RProc, env)]);
	     mov(dword [reg_tmp1 + OffsetOf(struct REnv, c)], reg_tmp0); */
	  /*
	          emit_cfunc_start(mrb, coi);
		  emit_local_var_type_read(mrb, coi, reg_tmp0s, dstno);
		  push(reg_tmp0s);
		  emit_local_var_value_read(mrb, coi, reg_tmp0s, dstno);
		  push(reg_tmp0s);
		  push(reg_mrb);
		  emit_call(mrb, coi, (void *)mrb_p);
		  emit_cfunc_end(mrb, coi, 12);
	  */
	  return code;
	}
      }
    }

    emit_cfunc_start(mrb, coi);
    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)mirep);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    if (flags & OP_L_CAPTURE) {
      emit_call(mrb, coi, (void *)mrb_closure_new);
    }
    else {
      emit_call(mrb, coi, (void *)mrb_proc_new);
    }
    emit_cfunc_end(mrb, coi, 2 * sizeof(void *));
    emit_load_literal(mrb, coi, reg_tmp1s, 0xfff00000 | MRB_TT_PROC);
    emit_local_var_type_write(mrb, coi, dstno, reg_tmp1s);
    if (flags & OP_L_STRICT) {
      emit_load_literal(mrb, coi, reg_tmp1, (cpu_word_t)MRB_PROC_STRICT);
      shl(reg_tmp1, 11);
      or(ptr [reg_tmp0], reg_tmp1);
    }
    emit_sub(mrb, coi, reg_tmp0, reg_mrb);
    emit_local_var_value_write(mrb, coi, dstno, reg_tmp0s);

    emit_move(mrb, coi, reg_tmp0s, reg_vars, VMSOffsetOf(ai));
    emit_move(mrb, coi, reg_mrb, OffsetOf(mrb_state, gc.arena_idx), reg_tmp0s);
    return code;
  }

  const void *
    ent_range(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstno = GETARG_A(**ppc);
    int srcno0 = GETARG_B(**ppc);
    int srcno1 = srcno0 + 1;
    int exelp = GETARG_C(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_RANGE;
    dinfo->klass = mrb_class(mrb, 
			     mrb_vm_const_get(mrb, mrb_intern_cstr(mrb, "Range")));
    dinfo->unboxedp = 0;

    emit_cfunc_start(mrb, coi);

    emit_load_literal(mrb, coi, reg_tmp0, exelp);
    emit_arg_push(mrb, coi, 3, reg_tmp0);
    emit_arg_push_nan(mrb, coi, 2, reg_tmp0, srcno1);
    emit_arg_push_nan(mrb, coi, 1, reg_tmp0, srcno0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_range_new);
    
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value) * 2 + sizeof(int));

    emit_local_var_write_from_cfunc(mrb, coi, dstno);
    return code;
  }

  const void *
    ent_string(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    mrb_irep *irep = *status->irep;
    int dstno = GETARG_A(**ppc);
    mrb_value *str = irep->pool + GETARG_Bx(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_STRING;
    dinfo->klass = mrb->string_class;
    dinfo->unboxedp = 0;

    if (GET_OPCODE(*(*ppc + 1)) == OP_STRCAT &&
	GETARG_B(*(*ppc + 1)) == GETARG_A(**ppc)) {
      emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)str);
      emit_move(mrb, coi, reg_tmp1, reg_tmp0, 4);
      emit_move(mrb, coi, reg_tmp0, reg_tmp0, 0);
    }
    else {
      emit_cfunc_start(mrb, coi);

      emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)str);
      emit_move(mrb, coi, reg_tmp1, reg_tmp0, 4);
      emit_arg_push(mrb, coi, 2, reg_tmp1);
      emit_move(mrb, coi, reg_tmp1, reg_tmp0, 0);
      emit_arg_push(mrb, coi, 1, reg_tmp1);
      emit_arg_push(mrb, coi, 0, reg_mrb);
      emit_call(mrb, coi, (void *)mrb_str_dup);
    
      emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value));
    }

    emit_local_var_write_from_cfunc(mrb, coi, dstno);

    return code;
  }

  const void *
    ent_strcat(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstno = GETARG_A(**ppc);
    int srcno = GETARG_B(**ppc);
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_STRING;
    dinfo->klass = mrb->string_class;
    dinfo->unboxedp = 0;

    emit_cfunc_start(mrb, coi);

    emit_arg_push_nan(mrb, coi, 2, reg_tmp0, srcno);
    emit_arg_push_nan(mrb, coi, 1, reg_tmp0, dstno);

    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_str_concat);
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value) * 2);

    return code;
  }

  const void *
    ent_hash(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrb_value *regs) 
  {
    const void *code = getCurr();
    mrb_code **ppc = status->pc;
    int dstno = GETARG_A(**ppc);
    int srcno = GETARG_B(**ppc);
    int num = GETARG_C(**ppc);
    int i;
    mrbjit_reginfo *dinfo = &coi->reginfo[GETARG_A(**ppc)];
    dinfo->type = MRB_TT_HASH;
    dinfo->klass = mrb->hash_class;
    dinfo->unboxedp = 0;

    gen_flush_regs(mrb, *ppc, status, coi, 1);
    emit_cfunc_start(mrb, coi);

    emit_load_literal(mrb, coi, reg_tmp0, (cpu_word_t)num);
    emit_arg_push(mrb, coi, 1, reg_tmp0);
    emit_arg_push(mrb, coi, 0, reg_mrb);
    emit_call(mrb, coi, (void *)mrb_hash_new_capa);
    emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(int));

    for (i = 0; i < num * 2; i+= 2) {
      emit_push(mrb, coi, reg_tmp0);
      emit_push(mrb, coi, reg_tmp1);
      emit_cfunc_start(mrb, coi);

      /* val */
      emit_arg_push_nan(mrb, coi, 3, reg_vars, srcno + i + 1);

      /* key */
      emit_arg_push_nan(mrb, coi, 2, reg_vars, srcno + i);

      /* hash */
      emit_arg_push_from_cfunc(mrb, coi, 1);

      /* mrb */
      emit_arg_push(mrb, coi, 0, reg_mrb);

      emit_call(mrb, coi, (void *)mrb_hash_set);

      emit_cfunc_end(mrb, coi, sizeof(mrb_state *) + sizeof(mrb_value) * 3);
      emit_pop(mrb, coi, reg_tmp1);
      emit_pop(mrb, coi, reg_tmp0);
    }

    emit_local_var_write_from_cfunc(mrb, coi, dstno);

    return code;
  }

  /* primitive helper */
  void gen_call_initialize(mrb_state *, mrb_value,mrbjit_vmstatus *, mrbjit_code_info *);

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
    mrbjit_prim_fix_and_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_fix_or_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_fix_lshift_impl(mrb_state *mrb, mrb_value proc,
			    mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_fix_rshift_impl(mrb_state *mrb, mrb_value proc,
			    mrbjit_vmstatus *status, mrbjit_code_info *coi);
  const void *
    mrbjit_prim_obj_not_equal_aux(mrb_state *mrb, mrb_value proc,
				  mrbjit_vmstatus *status, mrbjit_code_info *coi, mrbjit_reginfo *dinfo);

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
    mrbjit_prim_ary_first_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value 
    mrbjit_prim_ary_size_impl(mrb_state *mrb, mrb_value proc,
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
    mrbjit_prim_mmm_move_impl(mrb_state *mrb, mrb_value proc,
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
    mrbjit_prim_kernel_block_given_p_impl(mrb_state *mrb, mrb_value proc,
                              mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_math_sqrt_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_math_callcfunc_impl(mrb_state *mrb, mrb_value proc,
				    mrbjit_vmstatus *status, mrbjit_code_info *coi, void *);

  mrb_value
    mrbjit_prim_numeric_minus_at_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_str_plus_impl(mrb_state *mrb, mrb_value proc,
			      mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value
    mrbjit_prim_pvec4_new_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value
    mrbjit_prim_pvec4_add_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value
    mrbjit_prim_pvec4_sub_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);

  mrb_value
    mrbjit_prim_pvec4_aget_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);
  mrb_value
    mrbjit_prim_pvec4_aset_impl(mrb_state *mrb, mrb_value proc,
			       mrbjit_vmstatus *status, mrbjit_code_info *coi);
};



#endif  /* MRUBY_JITCODE_H */

#include "mruby/jitcode.h"

extern "C" {

void
mrbjit_gen_exit(mrbjit_code_area coderaw, mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->gen_exit(mrb, *ppc, 1, 0, status, coi);
}

const void *
mrbjit_gen_jump_block(mrbjit_code_area coderaw, mrb_state *mrb, void *entry, mrbjit_vmstatus *status, mrbjit_code_info *coi, mrbjit_code_info *prevcoi)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  return code->gen_jump_block(mrb, entry, status, coi, prevcoi);
}

const void *
mrbjit_get_curr(mrbjit_code_area coderaw)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  return code->getCurr();
}

static MRBJitCode *the_code = new MRBJitCode();

void
mrbjit_gen_jmp_patch(mrb_state *mrb, mrbjit_code_area coderaw, void *dst, void *target, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_jmp_patch(mrb, dst, target, status, coi);
}

void
mrbjit_gen_exit_patch(mrbjit_code_area coderaw, mrb_state *mrb, void *dst, mrb_code *pc, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_exit_patch(mrb, dst, pc, status, coi);
}

void
mrbjit_gen_exit_patch2(mrbjit_code_area coderaw, mrb_state *mrb, void *dst, mrb_code *pc, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_exit_patch2(mrb, dst, pc, status, coi);
}

void
mrbjit_gen_load_patch(mrbjit_code_area coderaw, mrb_state *mrb, void *dst, void *address, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_load_patch(mrb, dst, (cpu_word_t)address, status, coi);
}

void
mrbjit_gen_align(mrbjit_code_area coderaw, unsigned align)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_align(align);
}

static const void *
mrbjit_emit_code_aux(mrb_state *mrb, mrbjit_vmstatus *status,
		     MRBJitCode *code, mrbjit_code_info *coi)
{
  mrb_value *regs = mrb->c->stack;
  mrb_code **ppc = status->pc;
  const void *rc;
  const void *rc2 = NULL;
  const void *entry;

  if (code == NULL) {
    code = the_code;
    //    printf("%x \n", code->getCurr());
    mrb->compile_info.code_base = code;
  }

  if ((*status->irep)->iseq == *ppc && GET_OPCODE(**ppc) != OP_CALL) {
    /* Top of iseq */
    rc2 = code->ent_block_guard(mrb, status, coi);
    mrb->compile_info.force_compile = 0;
    mrb->compile_info.nest_level++;
  }

  entry = code->gen_entry(mrb, status);

#ifdef ENABLE_DEBUG
  if (mrb->code_fetch_hook) {
    code->gen_call_fetch_hook(mrb, status, coi);
  }
#endif

  if (mrb->compile_info.ignor_inst_cnt > 0) {
    return code->ent_nop(mrb, status, coi);
  }
    
  switch(GET_OPCODE(**ppc)) {
  case OP_NOP:
    rc =code->ent_nop(mrb, status, coi);
    break;
    
  case OP_MOVE:
    rc =code->ent_move(mrb, status, coi);
    break;

  case OP_LOADL:
    rc =code->ent_loadl(mrb, status, coi);
    break;

  case OP_LOADI:
    rc =code->ent_loadi(mrb, status, coi);
    break;

  case OP_LOADSYM:
    rc =code->ent_loadsym(mrb, status, coi);
    break;

  case OP_LOADSELF:
    rc =code->ent_loadself(mrb, status, coi);
    break;

  case OP_LOADNIL:
    rc =code->ent_loadnil(mrb, status, coi);
    break;

  case OP_LOADT:
    rc =code->ent_loadt(mrb, status, coi);
    break;

  case OP_LOADF:
    rc =code->ent_loadf(mrb, status, coi);
    break;

  case OP_GETGLOBAL:
    rc =code->ent_getglobal(mrb, status, coi);
    break;

  case OP_SETGLOBAL:
    rc =code->ent_setglobal(mrb, status, coi);
    break;

  case OP_GETIV:
    rc =code->ent_getiv(mrb, status, coi);
    break;

  case OP_SETIV:
    rc =code->ent_setiv(mrb, status, coi);
    break;


  case OP_GETCV:
    rc =code->ent_getcv(mrb, status, coi);
    break;

  case OP_SETCV:
    rc =code->ent_setcv(mrb, status, coi);
    break;

  case OP_GETCONST:
    rc =code->ent_getconst(mrb, status, coi);
    break;

  case OP_GETMCNST:
    rc =code->ent_getmconst(mrb, status, coi);
    break;

  case OP_SENDB:
  case OP_SEND:
    rc =code->ent_send(mrb, status, coi);
    break;

  case OP_CALL:
    rc = NULL;
    break;
    
  case OP_ENTER:
    rc =code->ent_enter(mrb, status, coi);
    break;

  case OP_RETURN:
    mrb->compile_info.nest_level--;
    if (mrb->c->ci->proc->env ||
	mrb->compile_info.nest_level < 0 ||
	(mrb->c->ci != mrb->c->cibase &&
	 mrb->c->ci[0].proc->body.irep == mrb->c->ci[-1].proc->body.irep)) {
      rc =code->ent_return(mrb, status, coi, **ppc);
    }
    else {
      rc =code->ent_return_inline(mrb, status, coi, **ppc);
    }
    break;

  case OP_TAILCALL:
    rc =code->ent_tailcall(mrb, status, coi);
    break;

  case OP_ADD:
    rc =code->ent_add(mrb, status, coi, regs);
    break;

  case OP_SUB:
    rc =code->ent_sub(mrb, status, coi, regs);
    break;

  case OP_MUL:
    rc =code->ent_mul(mrb, status, coi, regs);
    break;

  case OP_DIV:
    rc =code->ent_div(mrb, status, coi, regs);
    break;

  case OP_ADDI:
    rc =code->ent_addi(mrb, status, coi, regs);
    break;

  case OP_SUBI:
    rc =code->ent_subi(mrb, status, coi, regs);
    break;

  case OP_EQ:
    rc =code->ent_eq(mrb, status, coi, regs);
    break;

  case OP_LT:
    rc =code->ent_lt(mrb, status, coi, regs);
    break;

  case OP_LE:
    rc =code->ent_le(mrb, status, coi, regs);
    break;

  case OP_GT:
    rc =code->ent_gt(mrb, status, coi, regs);
    break;

  case OP_GE:
    rc =code->ent_ge(mrb, status, coi, regs);
    break;

  case OP_ARRAY:
    rc =code->ent_array(mrb, status, coi, regs);
    break;
    
  case OP_ARYCAT:
    rc =code->ent_arycat(mrb, status, coi, regs);
    break;
    
  case OP_AREF:
    rc =code->ent_aref(mrb, status, coi, regs);
    break;

  case OP_GETUPVAR:
    rc =code->ent_getupvar(mrb, status, coi);
    break;

  case OP_SETUPVAR:
    rc =code->ent_setupvar(mrb, status, coi);
    break;

  case OP_JMP:
    rc =code->ent_jmp(mrb, status, coi);
    break;

  case OP_JMPIF:
    rc =code->ent_jmpif(mrb, status, coi, regs);
    break;

  case OP_JMPNOT:
    rc =code->ent_jmpnot(mrb, status, coi, regs);
    break;

  case OP_ONERR:
    rc =code->ent_onerr(mrb, status, coi, regs);
    break;

  case OP_LAMBDA:
    rc =code->ent_lambda(mrb, status, coi, regs);
    break;

  case OP_RANGE:
    rc =code->ent_range(mrb, status, coi, regs);
    break;

  case OP_STRING:
    rc =code->ent_string(mrb, status, coi, regs);
    break;

  case OP_STRCAT:
    rc =code->ent_strcat(mrb, status, coi, regs);
    break;

  case OP_HASH:
    rc =code->ent_hash(mrb, status, coi, regs);
    break;

  case OP_BLKPUSH:
    rc = code->ent_blkpush(mrb, status, coi, regs);
    break;

  case OP_EPUSH:
    rc = code->ent_epush(mrb, status, coi, regs);
    break;

  default:
    mrb->compile_info.force_compile = 0;
    mrb->compile_info.nest_level = 0;
    rc = NULL;
    break;
  }

  if (rc == NULL) {
    /* delete fetch hook */
    code->set_entry(entry);
  }

  if (rc2) {
    if (rc == NULL) {
      code->gen_exit(mrb, *ppc, 1, 0, status, coi);
    }
    return rc2;
  }

  return rc;
}

const void *
mrbjit_emit_code(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;
  const void *rc;
  try {
    rc = mrbjit_emit_code_aux(mrb, status, code, coi);
  }
  catch(Xbyak::Error err) {
    printf("Xbyak error %d \n", int(err));
    exit(1);
  }
  if (rc == NULL && code == NULL) {
    mrb->compile_info.code_base = NULL;
  }

  return rc;
}
} /* extern "C" */

struct CodeDeleter {
 ~CodeDeleter()
  {
    delete the_code;
  }
} codeDeleter;

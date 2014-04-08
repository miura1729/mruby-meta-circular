#include "jitcode.h"

extern "C" {

void
mrbjit_gen_exit(mrbjit_code_area coderaw, mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrbjit_vmstatus *status)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->gen_exit(*ppc, 1, 0, status);
}

void
mrbjit_gen_jump_block(mrbjit_code_area coderaw, void *entry)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->gen_jump_block(entry);
}

const void *
mrbjit_get_curr(mrbjit_code_area coderaw)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  return code->getCurr();
}

static MRBJitCode *the_code = new MRBJitCode();

void
mrbjit_gen_jmp_patch(mrbjit_code_area coderaw, void *dst, void *target)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_jmp_patch(dst, target);
}

void
mrbjit_gen_exit_patch(mrbjit_code_area coderaw, void *dst, mrb_code *pc, mrbjit_vmstatus *status)
{
  MRBJitCode *code;
  if (coderaw == NULL) {
    code = the_code;
  } 
  else {
    code  = (MRBJitCode *) coderaw;
  }
  code->gen_exit_patch(dst, pc, status);
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
  mrb_value *regs = *status->regs;
  mrb_code **ppc = status->pc;
  const void *rc;

  if (code == NULL) {
    code = the_code;
    //    printf("%x \n", code->getCurr());
    mrb->compile_info.code_base = code;
  }
  const void *entry = code->gen_entry(mrb, status);

  if (mrb->code_fetch_hook) {
    code->gen_call_fetch_hook(mrb, status);
  }

  switch(GET_OPCODE(**ppc)) {
  case OP_NOP:
    rc =code->emit_nop(mrb, status, coi);
    break;
    
  case OP_MOVE:
    rc =code->emit_move(mrb, status, coi);
    break;

  case OP_LOADL:
    rc =code->emit_loadl(mrb, status, coi);
    break;

  case OP_LOADI:
    rc =code->emit_loadi(mrb, status, coi);
    break;

  case OP_LOADSYM:
    rc =code->emit_loadsym(mrb, status, coi);
    break;

  case OP_LOADSELF:
    rc =code->emit_loadself(mrb, status, coi);
    break;

  case OP_LOADNIL:
    rc =code->emit_loadnil(mrb, status, coi);
    break;

  case OP_LOADT:
    rc =code->emit_loadt(mrb, status, coi);
    break;

  case OP_LOADF:
    rc =code->emit_loadf(mrb, status, coi);
    break;

  case OP_GETIV:
    rc =code->emit_getiv(mrb, status, coi);
    break;

  case OP_SETIV:
    rc =code->emit_setiv(mrb, status, coi);
    break;


  case OP_GETCV:
    rc =code->emit_getcv(mrb, status, coi);
    break;

  case OP_SETCV:
    rc =code->emit_setcv(mrb, status, coi);
    break;

  case OP_GETCONST:
    rc =code->emit_getconst(mrb, status, coi);
    break;

  case OP_GETMCNST:
    rc =code->emit_getmconst(mrb, status, coi);
    break;

  case OP_SENDB:
  case OP_SEND:
    rc =code->emit_send(mrb, status, coi);
    break;

  case OP_CALL:
    //    rc =code->emit_call(mrb, status);
    if (mrb->compile_info.force_compile) {
      rc = code->emit_nop(mrb, status, coi);
    }
    else {
      rc = NULL;
    }
    break;
    
  case OP_ENTER:
    mrb->compile_info.nest_level++;
    rc =code->emit_enter(mrb, status, coi);
    break;

  case OP_RETURN:
    mrb->compile_info.nest_level--;
    if (mrb->c->ci->proc->env ||
	mrb->compile_info.nest_level < 0) {
      rc =code->emit_return(mrb, status, coi);
    }
    else {
      rc =code->emit_return_inline(mrb, status, coi);
    }
    break;

  case OP_ADD:
    rc =code->emit_add(mrb, status, coi, regs);
    break;

  case OP_SUB:
    rc =code->emit_sub(mrb, status, coi, regs);
    break;

  case OP_MUL:
    rc =code->emit_mul(mrb, status, coi, regs);
    break;

  case OP_DIV:
    rc =code->emit_div(mrb, status, coi, regs);
    break;

  case OP_ADDI:
    rc =code->emit_addi(mrb, status, coi, regs);
    break;

  case OP_SUBI:
    rc =code->emit_subi(mrb, status, coi, regs);
    break;

  case OP_EQ:
    rc =code->emit_eq(mrb, status, coi, regs);
    break;

  case OP_LT:
    rc =code->emit_lt(mrb, status, coi, regs);
    break;

  case OP_LE:
    rc =code->emit_le(mrb, status, coi, regs);
    break;

  case OP_GT:
    rc =code->emit_gt(mrb, status, coi, regs);
    break;

  case OP_GE:
    rc =code->emit_ge(mrb, status, coi, regs);
    break;

  case OP_ARRAY:
    rc =code->emit_array(mrb, status, coi, regs);
    break;
    
  case OP_GETUPVAR:
    rc =code->emit_getupvar(mrb, status, coi);
    break;

  case OP_SETUPVAR:
    rc =code->emit_setupvar(mrb, status, coi);
    break;

  case OP_JMP:
    rc =code->emit_jmp(mrb, status, coi);
    break;

  case OP_JMPIF:
    rc =code->emit_jmpif(mrb, status, coi, regs);
    break;

  case OP_JMPNOT:
    rc =code->emit_jmpnot(mrb, status, coi, regs);
    break;

  case OP_LAMBDA:
    rc =code->emit_lambda(mrb, status, coi, regs);
    break;

  case OP_RANGE:
    rc =code->emit_range(mrb, status, coi, regs);
    break;

  case OP_STRING:
    rc =code->emit_string(mrb, status, coi, regs);
    break;

  case OP_HASH:
    rc =code->emit_hash(mrb, status, coi, regs);
    break;

  default:
    mrb->compile_info.nest_level = 0;
    rc =NULL;
    break;
  }

  if (rc == NULL) {
    /* delete fetch hook */
    code->set_entry(entry);
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


#include "jitcode.h"

extern "C" {

void
mrbjit_gen_exit(mrbjit_code_area coderaw, mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->gen_exit(*ppc, 1, 0);
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

static const void *
mrbjit_emit_code_aux(mrb_state *mrb, mrbjit_vmstatus *status,
		     MRBJitCode *code, mrbjit_code_info *coi)
{
  mrb_value *regs = *status->regs;
  mrb_code **ppc = status->pc;
  const void *entry;

  if (code == NULL) {
    code = the_code;
    mrb->compile_info.code_base = code;
    entry = code->gen_entry(mrb, status);
  }

  switch(GET_OPCODE(**ppc)) {
  case OP_NOP:
    return code->emit_nop(mrb, status, coi);
    
  case OP_MOVE:
    return code->emit_move(mrb, status, coi);

  case OP_LOADL:
    return code->emit_loadl(mrb, status, coi);

  case OP_LOADI:
    return code->emit_loadi(mrb, status, coi);

  case OP_LOADSYM:
    return code->emit_loadsym(mrb, status, coi);

  case OP_LOADSELF:
    return code->emit_loadself(mrb, status, coi);

  case OP_LOADNIL:
    return code->emit_loadnil(mrb, status, coi);

  case OP_LOADT:
    return code->emit_loadt(mrb, status, coi);

  case OP_LOADF:
    return code->emit_loadf(mrb, status, coi);

  case OP_GETIV:
    return code->emit_getiv(mrb, status, coi);

  case OP_SETIV:
    return code->emit_setiv(mrb, status, coi);

  case OP_GETCV:
    return code->emit_getcv(mrb, status, coi);

  case OP_SETCV:
    return code->emit_setcv(mrb, status, coi);

  case OP_GETCONST:
    return code->emit_getconst(mrb, status, coi);

  case OP_SENDB:
  case OP_SEND:
    return code->emit_send(mrb, status, coi);

  case OP_CALL:
    return code->emit_call(mrb, status);
    
  case OP_ENTER:
    mrb->compile_info.nest_level++;
    return code->emit_enter(mrb, status);

  case OP_RETURN:
    mrb->compile_info.nest_level--;
    if (mrb->compile_info.nest_level < 0) {
      return code->emit_return(mrb, status);
    }
    else {
      return code->emit_return_inline(mrb, status);
    }

  case OP_ADD:
    return code->emit_add(mrb, status, coi, regs);

  case OP_SUB:
    return code->emit_sub(mrb, status, coi, regs);

  case OP_MUL:
    return code->emit_mul(mrb, status, coi, regs);

  case OP_DIV:
    return code->emit_div(mrb, status, coi, regs);

  case OP_ADDI:
    return code->emit_addi(mrb, status, coi, regs);

  case OP_SUBI:
    return code->emit_subi(mrb, status, coi, regs);

  case OP_EQ:
    return code->emit_eq(mrb, status, coi, regs);

  case OP_LT:
    return code->emit_lt(mrb, status, coi, regs);

  case OP_LE:
    return code->emit_le(mrb, status, coi, regs);

  case OP_GT:
    return code->emit_gt(mrb, status, coi, regs);

  case OP_GE:
    return code->emit_ge(mrb, status, coi, regs);

  case OP_GETUPVAR:
    return code->emit_getupvar(mrb, status, coi);

  case OP_SETUPVAR:
    return code->emit_setupvar(mrb, status, coi);

  case OP_JMP:
    return code->emit_jmp(mrb, status, coi);

  case OP_JMPIF:
    return code->emit_jmpif(mrb, status, coi, regs);

  case OP_JMPNOT:
    return code->emit_jmpnot(mrb, status, coi, regs);

    //  case OP_LAMBDA:
    //return code->emit_lambda(mrb, status, coi, regs);

  default:
    mrb->compile_info.nest_level = 0;
    return NULL;
  }
}

const void *
mrbjit_emit_code(mrb_state *mrb, mrbjit_vmstatus *status, mrbjit_code_info *coi)
{
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;
  const void *rc = mrbjit_emit_code_aux(mrb, status, code, coi);
  if (rc == NULL && code == NULL) {
    mrb->compile_info.code_base = NULL;
  }

  return rc;
}
} /* extern "C" */


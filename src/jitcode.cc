#include "jitcode.h"

extern "C" {

void
mrbjit_gen_exit(mrbjit_code_area coderaw, mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->gen_exit(*ppc);
}

void
mrbjit_gen_jump_block(mrbjit_code_area coderaw, void *entry)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->gen_jump_block(entry);
}

static MRBJitCode *the_code = new MRBJitCode();

const void *
mrbjit_emit_code(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_irep *irep = *status->irep;
  MRBJitCode *code = (MRBJitCode *)mrb->compile_info.code_base;
  mrb_value *regs = *status->regs;
  mrb_code **ppc = status->pc;
  const void *entry;

  if (code == NULL) {
    code = the_code;
    mrb->compile_info.code_base = code;
    entry = code->gen_entry(mrb, irep);
  }

  switch(GET_OPCODE(**ppc)) {
  case OP_NOP:
    return code->emit_nop(mrb, irep, ppc);
    
  case OP_MOVE:
    return code->emit_move(mrb, irep, ppc);

  case OP_LOADL:
    return code->emit_loadl(mrb, irep, ppc);

  case OP_LOADI:
    return code->emit_loadi(mrb, irep, ppc);

    // OP_LOADSYM

  case OP_LOADSELF:
    return code->emit_loadself(mrb, irep, ppc);

  case OP_LOADNIL:
    return code->emit_loadnil(mrb, irep, ppc);

  case OP_LOADT:
    return code->emit_loadt(mrb, irep, ppc);

  case OP_LOADF:
    return code->emit_loadf(mrb, irep, ppc);

  case OP_GETIV:
    return code->emit_getiv(mrb, irep, ppc);

  case OP_SETIV:
    return code->emit_setiv(mrb, irep, ppc);

  case OP_GETCONST:
    return code->emit_getconst(mrb, irep, ppc);

  case OP_SENDB:
  case OP_SEND:
    return code->emit_send(mrb, status);

  case OP_ENTER:
    return code->emit_enter(mrb, status);

  case OP_RETURN:
    return code->emit_return(mrb, status);

  case OP_ADD:
    return code->emit_add(mrb, irep, ppc, regs);

  case OP_SUB:
    return code->emit_sub(mrb, irep, ppc, regs);

  case OP_MUL:
    return code->emit_mul(mrb, irep, ppc, regs);

    //case OP_DIV:
    //return code->emit_div(mrb, irep, ppc, regs);

  case OP_ADDI:
    return code->emit_addi(mrb, irep, ppc, regs);

  case OP_SUBI:
    return code->emit_subi(mrb, irep, ppc, regs);

  case OP_EQ:
    return code->emit_eq(mrb, irep, ppc, regs);

  case OP_LT:
    return code->emit_lt(mrb, irep, ppc, regs);

  case OP_LE:
    return code->emit_le(mrb, irep, ppc, regs);

  case OP_GT:
    return code->emit_gt(mrb, irep, ppc, regs);

  case OP_GE:
    return code->emit_ge(mrb, irep, ppc, regs);

  case OP_GETUPVAR:
    return code->emit_getupvar(mrb, irep, ppc);

  case OP_SETUPVAR:
    return code->emit_setupvar(mrb, irep, ppc);

  case OP_JMP:
    return code->emit_jmp(mrb, irep, ppc);

  case OP_JMPIF:
    return code->emit_jmpif(mrb, irep, ppc, regs);

  case OP_JMPNOT:
    return code->emit_jmpnot(mrb, irep, ppc, regs);

  default:
    mrb->compile_info.nest_level = 0;
    return NULL;
  }
}

} /* extern "C" */


#include "jitcode.h"

extern "C" {
#include "mruby.h"
#include "mruby/jit.h"

const void *
mrbjit_emit_entry(mrbjit_code_area coderaw, mrb_state *mrb, mrb_irep *irep)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  return code->emit_entry(mrb, irep);
}

void
mrbjit_emit_exit(mrbjit_code_area coderaw, mrb_state *mrb, mrb_irep *irep)
{
  MRBJitCode *code = (MRBJitCode *) coderaw;
  code->emit_exit();
}

const void *
mrbjit_emit_code(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc)
{
  MRBJitCode *code = (MRBJitCode *) irep->compile_info->code_base;
  const void *entry;

  if (code == NULL) {
    code = new MRBJitCode();
    irep->compile_info->code_base = code;
    entry = code->emit_entry(mrb, irep);
  }
  else {
    entry = (void *)1;		/* dummy for mark of using */
  }

  code->emit_mov();

  return entry;
}

} /* extern "C" */


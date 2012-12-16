#include "jitcode.h"

extern "C" {
#include "mruby.h"
#include "mruby/jit.h"

void 
emit_code(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  MRBJitCode code;
  code.emit_mov();
}
} /* extern "C" */

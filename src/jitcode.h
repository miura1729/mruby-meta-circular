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
#include "mruby/jit.h"
} /* extern "C" */

/* Regs Map                               *
 * ebp   -- pointer to regs               *
 * ecx   -- pointer to pool               *
 * ebx   -- pointer to pc                 */
class MRBJitCode: public Xbyak::CodeGenerator {

 public:

 MRBJitCode():
  CodeGenerator(1024)
  {
  }

  const void *emit_entry(mrb_state *mrb, mrb_irep *irep) {
    const void* func_ptr = getCurr();
    return func_ptr;
  }

  void emit_exit() {
    ret();
  }
  
  const void *emit_mov(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) {
    const void *code = getCurr();

    return code;
  }

  const void *emit_loadl(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc) {
    const void *code = getCurr();

    return code;
  }
};

#endif  /* MRUBY_JITCODE_H */

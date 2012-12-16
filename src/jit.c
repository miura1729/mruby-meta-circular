/*
** jit.c - Toplevel of JIT
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/array.h"
#include <stddef.h>
#include <stdio.h>

#define ISEQ_OFFSET_OF(pc) ((size_t)((pc) - irep->iseq))

extern void emit_code(mrb_state *, mrb_irep *, mrb_code **, mrb_value *);

void
mrbjit_dispatch(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  size_t n = ISEQ_OFFSET_OF(*ppc);
  irep->prof_info[n]++;
  irep->compile_info->prev_pc = *ppc;
  emit_code(mrb, irep, ppc, regs);
}

void
mrbjit_dispatch_local_jump(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  mrbjit_dispatch(mrb, irep, ppc, regs);
  
}

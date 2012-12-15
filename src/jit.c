#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/array.h"
#include <stddef.h>
#include <stdio.h>

#define ISEQ_OFFSET_OF(pc) ((size_t)((pc) - irep->iseq))

void
mrbjit_dispatch(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  size_t n = ISEQ_OFFSET_OF(*ppc);
  irep->prof_info[n]++;
  irep->compile_info->prev_pc = *ppc;
}

void
mrbjit_dispatch_local_jump(mrb_state *mrb, mrb_irep *irep, mrb_code **ppc, mrb_value *regs)
{
  mrbjit_dispatch(mrb, irep, ppc, regs);
  
}

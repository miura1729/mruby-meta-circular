#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/dump.h"
#include "mruby/string.h"
#include "mruby/proc.h"

extern const char vmopt_irep[];

void
mrb_init_vmopt(mrb_state *mrb)
{
  int n = mrb_read_irep(mrb, vmopt_irep);

  extern mrb_value mrb_top_self(mrb_state *mrb);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
}

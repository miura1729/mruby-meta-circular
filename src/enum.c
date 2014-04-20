/*
** enum.c - Enumerable module
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/primitive.h"

void
mrb_init_enumerable(mrb_state *mrb)
{
  struct RClass *mod;
  mod = mrb_define_module(mrb, "Enumerable");  /* 15.3.2 */
  //mrbjit_define_primitive(mrb, mod, "all?", mrbjit_prim_enum_all);
}


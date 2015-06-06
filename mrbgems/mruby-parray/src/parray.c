#include "mruby.h"
#include "mruby/class.h"
#include "mruby/value.h"
#include "mruby/variable.h"
#include "mruby/primitive.h"
#include "prim.h"

void
mrb_mruby_parray_gem_init(mrb_state *mrb)
{
  struct RClass *parray;
  struct RClass *vec4;

  parray = mrb_define_module(mrb, "PArray");
  vec4 = mrb_define_class_under(mrb, parray, "PVector4", mrb->object_class);

  mrbjit_define_primitive(mrb, vec4, "+", mrbjit_prim_pve4_add);
}

void
mrb_mruby_parray_gem_final(mrb_state *mrb)
{
}

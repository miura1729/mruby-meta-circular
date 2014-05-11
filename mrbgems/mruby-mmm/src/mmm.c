#include "mruby.h"
#include "mruby/class.h"
#include "mruby/value.h"
#include "mruby/variable.h"
#include "mruby/primitive.h"

static mrb_value
mrb_mmm_move(mrb_state *mrb, mrb_value self)
{
  struct RObject *obj = mrb_obj_ptr(self);
  struct RObject *cls = (struct RObject *)obj->c;

  mrb_obj_iv_set(mrb, cls, mrb_intern_lit(mrb, "__objcache__"), self);

  return self;
}

static mrb_value
mrb_mmm_instance_new(mrb_state *mrb, mrb_value self)
{
  struct RObject *obj = mrb_obj_ptr(self);
  mrb_value ins = mrb_obj_iv_get(mrb, obj, mrb_intern_lit(mrb, "__objcache__"));
  mrb_value blk;
  mrb_value *argv;
  mrb_int argc;

  if (mrb_nil_p(ins)) {
    return mrb_instance_new(mrb, self);
  }
  else {
    mrb_get_args(mrb, "*&", &argv, &argc, &blk);
    mrb_funcall_with_block(mrb, ins, mrb_intern_lit(mrb, "initialize"), argc, argv, blk);
    mrb_obj_iv_set(mrb, obj, mrb_intern_lit(mrb, "__objcache__"), mrb_nil_value());

    return ins;
  }
}

void
mrb_mruby_mmm_gem_init(mrb_state *mrb)
{
  struct RClass *mmmi;
  struct RClass *mmmc;

  mmmi = mrb_define_module(mrb, "MMMI");
  mrb_define_method(mrb, mmmi, "move", mrb_mmm_move, MRB_ARGS_NONE());

  mmmc = mrb_define_module(mrb, "MMMC");
  mrb_define_method(mrb, mmmc, "new", mrb_mmm_instance_new, MRB_ARGS_ANY());
  mrbjit_define_primitive(mrb, mmmc, "new", mrbjit_prim_mmm_instance_new);
}

void
mrb_mruby_mmm_gem_final(mrb_state *mrb)
{
}

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
  mrb_sym objcache_sym = mrb_intern_lit(mrb, "__objcache__");
  mrb_value oldobj;

  oldobj = mrb_obj_iv_get(mrb, cls, objcache_sym);
  obj->c = mrb_class_ptr(oldobj);
  mrb_obj_iv_set(mrb, cls, objcache_sym, self);

  return self;
}

static mrb_value
mrb_mmm_instance_new(mrb_state *mrb, mrb_value self)
{
  struct RObject *cls = mrb_obj_ptr(self);
  mrb_sym objcache_sym = mrb_intern_lit(mrb, "__objcache__");
  mrb_value ins = mrb_obj_iv_get(mrb, cls, objcache_sym);
  mrb_value blk;
  mrb_value *argv;
  mrb_int argc;

  if (mrb_nil_p(ins)) {
    return mrb_instance_new(mrb, self);
  }
  else {
    if (mrb_obj_ptr(ins)->c) {
      mrb_obj_iv_set(mrb, cls, objcache_sym, mrb_obj_value(mrb_obj_ptr(ins)->c));
    }
    else {
      mrb_obj_iv_set(mrb, cls, objcache_sym, mrb_nil_value());
    }
    mrb_obj_ptr(ins)->c = cls;
    mrb_get_args(mrb, "*&", &argv, &argc, &blk);
    mrb_obj_ptr(ins)->iv->last_len = 0;
    mrb_funcall_with_block(mrb, ins, mrb_intern_lit(mrb, "initialize"), argc, argv, blk);

    return ins;
  }
}

mrb_value
mrb_mmm_included(mrb_state *mrb, mrb_value self)
{
  mrb_value klass;
  struct RClass *clsptr;
  
  mrb_get_args(mrb, "o", &klass);
  clsptr = mrb_class_ptr(klass);
  mrb_define_class_method(mrb, clsptr, "new", mrb_mmm_instance_new, MRB_ARGS_ANY());

  mrbjit_define_primitive(mrb, clsptr->c, "new", mrbjit_prim_mmm_instance_new);
  return mrb_nil_value();
}

void
mrb_mruby_mmm_gem_init(mrb_state *mrb)
{
  struct RClass *mmm;

  mmm = mrb_define_module(mrb, "MMM");
  mrb_define_method(mrb, mmm, "move", mrb_mmm_move, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, mmm, "included", mrb_mmm_included, MRB_ARGS_REQ(1));
  mrbjit_define_primitive(mrb, mmm, "move", mrbjit_prim_mmm_move);
}

void
mrb_mruby_mmm_gem_final(mrb_state *mrb)
{
}

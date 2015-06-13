#include "mruby.h"
#include "mruby/class.h"
#include "mruby/value.h"
#include "mruby/array.h"
#include "mruby/variable.h"
#include "mruby/primitive.h"
#include "prim.h"

static mrb_value
mrb_vec_instance_new(mrb_state *mrb, mrb_value self)
{
  struct RObject *cls = mrb_obj_ptr(self);
  mrb_value ins = mrb_obj_iv_get(mrb, cls, mrb_intern_lit(mrb, "__objcache__"));
  mrb_value blk;
  mrb_value *argv;
  mrb_int argc;

  if (mrb_nil_p(ins)) {
    return mrb_instance_new(mrb, self);
  }
  else {
    if (mrb_obj_ptr(ins)->c) {
      mrb_obj_iv_set(mrb, cls, mrb_intern_lit(mrb, "__objcache__"), 
		     mrb_obj_value(mrb_obj_ptr(ins)->c));
    }
    else {
      mrb_obj_iv_set(mrb, cls, mrb_intern_lit(mrb, "__objcache__"), mrb_nil_value());
    }
    mrb_obj_ptr(ins)->c = cls;
    mrb_get_args(mrb, "*&", &argv, &argc, &blk);
    mrb_funcall_with_block(mrb, ins, mrb_intern_lit(mrb, "initialize"), argc, argv, blk);

    return ins;
  }
}

static mrb_value
mrb_vec_aget(mrb_state *mrb, mrb_value self)
{
  mrb_value index;
  struct RArray *a = mrb_ary_ptr(self);

  mrb_get_args(mrb, "o", &index);
  return a->ptr[mrb_fixnum(index)];
}

static mrb_value
mrb_vec_aset(mrb_state *mrb, mrb_value self)
{
  mrb_value v1, v2;

  mrb_get_args(mrb, "oo", &v1, &v2);
  mrb_ary_set(mrb, self, mrb_fixnum(v1), v2);

  return v2;
}

void
mrb_mruby_parray_gem_init(mrb_state *mrb)
{
  struct RClass *parray;
  struct RClass *vec4;

  parray = mrb_define_module(mrb, "PArray");
  vec4 = mrb_define_class_under(mrb, parray, "PVector4", mrb->object_class);
  MRB_SET_INSTANCE_TT(vec4, MRB_TT_ARRAY);

  mrb_define_class_method(mrb, vec4, "[]",        mrb_vec_instance_new,     MRB_ARGS_ANY());

  mrb_define_method(mrb, vec4, "[]",              mrb_vec_aget,         MRB_ARGS_ANY());
  mrb_define_method(mrb, vec4, "[]=",             mrb_vec_aset,         MRB_ARGS_ANY());
  mrbjit_define_primitive(mrb, vec4, "+", mrbjit_prim_pve4_add);
}

void
mrb_mruby_parray_gem_final(mrb_state *mrb)
{
}

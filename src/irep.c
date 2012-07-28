#include "mruby.h"
#ifdef ENABLE_IREP
#include "mruby/class.h"
#include "mruby/data.h"
#include "opcode.h"
#include "mruby/irep.h"

static void
mrb_irep_free(mrb_state *mrb, void *ptr)
{
  /* Do nothing */
}

static struct mrb_data_type mrb_irep_type = { "Irep", mrb_irep_free };

static mrb_value
mrb_irep_wrap(mrb_state *mrb, struct RClass *tc, struct mrb_irep *irep)
{
  return mrb_obj_value(Data_Wrap_Struct(mrb, tc, &mrb_irep_type, irep));
}

static mrb_value
mrb_irep_get_irep_by_no(mrb_state *mrb, mrb_value self)
{
  mrb_value no;
  mrb_get_args(mrb, "i", &no);

  return mrb_irep_wrap(mrb, mrb_class_ptr(self), mrb->irep[no.value.i]);
}

void
mrb_init_irep(mrb_state *mrb)
{
  struct RClass *a;

  a = mrb->array_class = mrb_define_class(mrb, "Irep", mrb->object_class);
  MRB_SET_INSTANCE_TT(a, MRB_TT_DATA);

  mrb_define_class_method(mrb, a, "get_irep_by_no", mrb_irep_get_irep_by_no,     ARGS_REQ(1));
}

#endif /* ENABLE_IREP */

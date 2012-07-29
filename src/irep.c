#include "mruby.h"
#ifdef ENABLE_IREP
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/array.h"
#include "mruby/proc.h"
#include "opcode.h"
#include "mruby/irep.h"

static char *optable[] = {
  "NOP", "MOVE",
  "LOADL", "LOADI", "LOADSYM", "LOADNIL",
  "LOADSELF", "LOADT", "LOADF",
  "GETGLOBAL", "SETGLOBAL", "GETSPECIAL", "SETSPECIAL",
  "GETIV", "SETIV", "GETCV", "SETCV",
  "GETCONST", "SETCONST", "GETMCNST", "SETMCNST",
  "GETUPVAR", "SETUPVAR",
  "JMP", "JMPIF", "JMPNOT",
  "ONERR", "RESCUE", "POPERR", "RAISE", "EPUSH", "EPOP",
  "SEND", "FSEND", "VSEND",
  "CALL", "SUPER", "ARGARY", "ENTER",
  "KARG", "KDICT", "RETURN", "TAILCALL", "BLKPUSH",
  "ADD", "ADDI", "SUB", "SUBI", "MUL", "DIV",
  "EQ", "LT", "LE", "GT", "GE",
  "ARRAY", "ARYCAT", "ARYPUSH", "AREF", "ASET", "APOST",
  "STRING", "STRCAT", "HASH",
  "LAMBDA", "RANGE", "OCLASS",
  "CLASS", "MODULE", "EXEC",
  "METHOD", "SCLASS", "TCLASS",
  "DEBUG", "STOP", "ERR",
};

static mrb_value
mrb_irep_make_optab(mrb_state *mrb)
{
  int i;
  int siz = sizeof(optable) / sizeof(optable[0]);
  mrb_value ary = mrb_ary_new_capa(mrb, siz);

  for (i = 0; i < siz; i++) {
    int ai = mrb_gc_arena_save(mrb);
    mrb_ary_push(mrb, ary, mrb_str_new2(mrb, optable[i]));
    mrb_gc_arena_restore(mrb, ai);
  }

  return ary;
}

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

  return mrb_irep_wrap(mrb, mrb_class_ptr(self), mrb->irep[mrb_fixnum(no)]);
}

static mrb_value
mrb_irep_get_irep(mrb_state *mrb, mrb_value self)
{
  mrb_value recv, name;
  struct RProc *m;
  struct RClass *c;

  mrb_get_args(mrb, "oo", &recv, &name);
  c = mrb_class(mrb, recv);
  m = mrb_method_search_vm(mrb, &c, mrb_symbol(name));

  if (m) {
    return mrb_irep_wrap(mrb, mrb_class_ptr(self), m->body.irep);
  }
  else {
    return mrb_nil_value();
  }
}

static mrb_value
mrb_irep_iseq(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value ary = mrb_ary_new_capa(mrb, irep->ilen);

  
  for (i = 0; i < irep->ilen; i++) {
    mrb_ary_push(mrb, ary, mrb_fixnum_value((mrb_int) irep->iseq[i]));
  }

  return ary;
}

static mrb_value
mrb_irep_nregs(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);

  return mrb_fixnum_value((mrb_int) irep->nregs);
}

static mrb_value
mrb_irep_nlocals(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);

  return mrb_fixnum_value((mrb_int) irep->nlocals);
}

void
mrb_init_irep(mrb_state *mrb)
{
  struct RClass *a;

  a = mrb_define_class(mrb, "Irep", mrb->object_class);
  MRB_SET_INSTANCE_TT(a, MRB_TT_DATA);

  mrb_define_const(mrb, a, "OPTABLE", mrb_irep_make_optab(mrb));
  mrb_define_class_method(mrb, a, "get_irep_by_no", mrb_irep_get_irep_by_no, ARGS_REQ(1));
  mrb_define_class_method(mrb, a, "get_irep", mrb_irep_get_irep, ARGS_REQ(2));

  mrb_define_method(mrb, a, "iseq", mrb_irep_iseq,     ARGS_NONE());
  mrb_define_method(mrb, a, "nregs", mrb_irep_nregs,     ARGS_NONE());
  mrb_define_method(mrb, a, "nlocals", mrb_irep_nlocals,     ARGS_NONE());
}

#endif /* ENABLE_IREP */

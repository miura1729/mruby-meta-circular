#include "mruby.h"
#ifdef ENABLE_IREP
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/array.h"
#include "mruby/proc.h"
#include "opcode.h"
#include "mruby/irep.h"
#include <stdio.h>

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
  "SEND", "SENDB", "FSEND",
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

void
disasm_irep(mrb_state *mrb, mrb_irep *irep, mrb_code c)
{
  int i = 0;
  switch (GET_OPCODE(c)) {
  case OP_NOP:
    printf("OP_NOP\n");
    break;
  case OP_MOVE:
    printf("OP_MOVE\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
    break;
  case OP_LOADL:
    printf("OP_LOADL\tR%d\tL(%d)\n", GETARG_A(c), GETARG_Bx(c));
    break;
  case OP_LOADI:
    printf("OP_LOADI\tR%d\t%d\n", GETARG_A(c), GETARG_sBx(c));
    break;
  case OP_LOADSYM:
    printf("OP_LOADSYM\tR%d\t:%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
    break;
  case OP_LOADNIL:
    printf("OP_LOADNIL\tR%d\n", GETARG_A(c));
    break;
  case OP_LOADSELF:
    printf("OP_LOADSELF\tR%d\n", GETARG_A(c));
    break;
  case OP_LOADT:
    printf("OP_LOADT\tR%d\n", GETARG_A(c));
    break;
  case OP_LOADF:
    printf("OP_LOADF\tR%d\n", GETARG_A(c));
    break;
  case OP_GETGLOBAL:
    printf("OP_GETGLOBAL\tR%d\t:%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
    break;
  case OP_SETGLOBAL:
    printf("OP_SETGLOBAL\t:%s\tR%d\n",
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
	   GETARG_A(c));
    break;
  case OP_GETCONST:
    printf("OP_GETCONST\tR%d\t:%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
    break;
  case OP_SETCONST:
    printf("OP_SETCONST\t:%s\tR%d\n",
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
	   GETARG_A(c));
    break;
  case OP_GETMCNST:
    printf("OP_GETMCNST\tR%d\tR%d::%s\n", GETARG_A(c), GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
    break;
  case OP_SETMCNST:
    printf("OP_SETMCNST\tR%d::%s\tR%d\n", GETARG_A(c)+1,
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
	   GETARG_A(c));
    break;
  case OP_GETIV:
    printf("OP_GETIV\tR%d\t%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
    break;
  case OP_SETIV:
    printf("OP_SETIV\t%s\tR%d\n",
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
	   GETARG_A(c));
    break;
  case OP_GETUPVAR:
    printf("OP_GETUPVAR\tR%d\t%d\t%d\n",
	   GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  case OP_SETUPVAR:
    printf("OP_SETUPVAR\tR%d\t%d\t%d\n",
	   GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  case OP_GETCV:
    printf("OP_GETCV\tR%d\t%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
    break;
  case OP_SETCV:
    printf("OP_SETCV\t%s\tR%d\n",
	   mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
	   GETARG_A(c));
    break;
  case OP_JMP:
    printf("OP_JMP\t\t%03d\n", i+GETARG_sBx(c));
    break;
  case OP_JMPIF:
    printf("OP_JMPIF\tR%d\t%03d\n", GETARG_A(c), i+GETARG_sBx(c));
    break;
  case OP_JMPNOT:
    printf("OP_JMPNOT\tR%d\t%03d\n", GETARG_A(c), i+GETARG_sBx(c));
    break;
  case OP_SEND:
    printf("OP_SEND\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_SENDB:
    printf("OP_SENDB\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_FSEND:
    printf("OP_FSEND\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_CALL:
    printf("OP_CALL\tR%d\n", GETARG_A(c));
    break;
  case OP_TAILCALL:
    printf("OP_TAILCALL\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_SUPER:
    printf("OP_SUPER\tR%d\t%d\n", GETARG_A(c),
	   GETARG_C(c));
    break;
  case OP_ARGARY:
    printf("OP_ARGARY\tR%d\t%d:%d:%d:%d\n", GETARG_A(c),
	   (GETARG_Bx(c)>>10)&0x3f,
	   (GETARG_Bx(c)>>9)&0x1,
	   (GETARG_Bx(c)>>4)&0x1f,
	   (GETARG_Bx(c)>>0)&0xf);
    break;

  case OP_ENTER:
    printf("OP_ENTER\t%d:%d:%d:%d:%d:%d:%d\n",
	   (GETARG_Ax(c)>>18)&0x1f,
	   (GETARG_Ax(c)>>13)&0x1f,
	   (GETARG_Ax(c)>>12)&0x1,
	   (GETARG_Ax(c)>>7)&0x1f,
	   (GETARG_Ax(c)>>2)&0x1f,
	   (GETARG_Ax(c)>>1)&0x1,
	   GETARG_Ax(c) & 0x1);
    break;
  case OP_RETURN:
    printf("OP_RETURN\tR%d", GETARG_A(c));
    switch (GETARG_B(c)) {
    case OP_R_NORMAL:
      printf("\n"); break;
    case OP_R_RETURN:
      printf("\treturn\n"); break;
    case OP_R_BREAK:
      printf("\tbreak\n"); break;
    default:
      printf("\tbroken\n"); break;
      break;
    }
    break;
  case OP_BLKPUSH:
    printf("OP_BLKPUSH\tR%d\t%d:%d:%d:%d\n", GETARG_A(c),
	   (GETARG_Bx(c)>>10)&0x3f,
	   (GETARG_Bx(c)>>9)&0x1,
	   (GETARG_Bx(c)>>4)&0x1f,
	   (GETARG_Bx(c)>>0)&0xf);
    break;

  case OP_LAMBDA:
    printf("OP_LAMBDA\tR%d\tI(%+d)\t%d\n", GETARG_A(c), GETARG_b(c), GETARG_c(c));
    break;
  case OP_RANGE:
    printf("OP_RANGE\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  case OP_METHOD:
    printf("OP_METHOD\tR%d\t:%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
    break;

  case OP_ADD:
    printf("OP_ADD\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_ADDI:
    printf("OP_ADDI\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_SUB:
    printf("OP_SUB\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_SUBI:
    printf("OP_SUBI\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_MUL:
    printf("OP_MUL\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_DIV:
    printf("OP_DIV\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_LT:
    printf("OP_LT\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_LE:
    printf("OP_LE\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_GT:
    printf("OP_GT\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_GE:
    printf("OP_GE\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;
  case OP_EQ:
    printf("OP_EQ\tR%d\t:%s\t%d\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
	   GETARG_C(c));
    break;

  case OP_STOP:
    printf("OP_STOP\n");
    break;

  case OP_ARRAY:
    printf("OP_ARRAY\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  case OP_ARYCAT:
    printf("OP_ARYCAT\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
    break;
  case OP_ARYPUSH:
    printf("OP_ARYPUSH\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
    break;
  case OP_AREF:
    printf("OP_AREF\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  case OP_APOST:
    printf("OP_APOST\tR%d\t%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  case OP_STRING:
    {
      /*      mrb_value s = irep->pool[GETARG_Bx(c)];
	
      s = mrb_str_dump(mrb, s);
      printf("OP_STRING\tR%d\t%s\n", GETARG_A(c), RSTRING_PTR(s));*/
      printf("OP_STRING\n");
    }
    break;
  case OP_STRCAT:
    printf("OP_STRCAT\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
    break;
  case OP_HASH:
    printf("OP_HASH\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;

  case OP_OCLASS:
    printf("OP_OCLASS\tR%d\n", GETARG_A(c));
    break;
  case OP_CLASS:
    printf("OP_CLASS\tR%d\t:%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
    break;
  case OP_MODULE:
    printf("OP_MODULE\tR%d\t:%s\n", GETARG_A(c),
	   mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
    break;
  case OP_EXEC:
    printf("OP_EXEC\tR%d\tI(%d)\n", GETARG_A(c), GETARG_Bx(c));
    break;
  case OP_SCLASS:
    printf("OP_SCLASS\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
    break;
  case OP_TCLASS:
    printf("OP_TCLASS\tR%d\n", GETARG_A(c));
    break;
  case OP_ERR:
    printf("OP_ERR\tL(%d)\n", GETARG_Bx(c));
    break;
  case OP_EPUSH:
    printf("OP_EPUSH\t:I(%d)\n", GETARG_Bx(c));
    break;
  case OP_ONERR:
    printf("OP_ONERR\t%03d\n", i+GETARG_sBx(c));
    break;
  case OP_RESCUE:
    printf("OP_RESCUE\tR%d\n", GETARG_A(c));
    break;
  case OP_RAISE:
    printf("OP_RAISE\tR%d\n", GETARG_A(c));
    break;
  case OP_POPERR:
    printf("OP_POPERR\t%d\n", GETARG_A(c));
    break;
  case OP_EPOP:
    printf("OP_EPOP\t%d\n", GETARG_A(c));
    break;

  default:
    printf("OP_unknown %d\t%d\t%d\t%d\n", GET_OPCODE(c),
	   GETARG_A(c), GETARG_B(c), GETARG_C(c));
    break;
  }
}

static mrb_value
mrb_irep_make_optab(mrb_state *mrb)
{
  int i;
  int siz = sizeof(optable) / sizeof(optable[0]);
  mrb_value ary = mrb_ary_new_capa(mrb, siz);

  for (i = 0; i < siz; i++) {
    int ai = mrb_gc_arena_save(mrb);
    mrb_ary_push(mrb, ary, mrb_str_new(mrb, optable[i], strlen(optable[i])));
    mrb_gc_arena_restore(mrb, ai);
  }

  return ary;
}

static void
mrb_irep_free2(mrb_state *mrb, void *ptr)
{
  /* Do nothing */
}

static struct mrb_data_type mrb_irep_type = { "Irep", mrb_irep_free2 };

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

#include "mruby.h"
#include "mruby/primitive.h"

struct irep_link {
  mrb_irep *irep;
  struct irep_link prev;
};

static void
mrb_simdcode_free(mrb_state *mrb, void *ptr)
{
  /* Do nothing */
}

static struct mrb_data_type mrb_simdcode_type = { "SIMD", mrb_simdcode_free };

static void
simd_init_codeinfo(mrb_state *mrb, mrbjit_code_info *ci, mrb_irep *irep)
{
  int j;

  ci->prev_pc = NULL;
  ci->method_arg_ver = 0;
  ci->caller_pc = NULL;
  ci->entry = NULL;
  ci->used = 1;

  ci->reginfo = (mrbjit_reginfo *)mrb_calloc(mrb, irep->nregs + 1, sizeof(mrbjit_reginfo));
  for (j = 0; j < irep->nregs + 1; j++) {
    ci->reginfo[j].type = MRB_TT_FREE;
    ci->reginfo[j].klass = NULL;
    ci->reginfo[j].constp = 0;
    ci->reginfo[j].unboxedp = 1;
    ci->reginfo[j].regplace = MRBJIT_REG_MEMORY;
  }
}

static mrb_value
mrbjit_simd_analyze(mrb_state *mrb, mrb_irep irep, struct irep_link *pnode)
{
  int i, j;
  struct mrbjit_code_info *ci;
  struct irep_link node;

  node.irep = irep;
  node.prev = pnode;

  ci = mrbjit_add_codeinfo(mrb, irep->jit_entry_tab + i, irep);
  simd_init_codeinfo(mrb, ci, irep);

  for (i = 0; i < irep->ilen; i++) {
    /* Same registor info for all instruction */
    mrb_code ins;
    irep->jit_entry_tab[i].body[0] = ci;

    ins = irep->iseq[i];

    switch (GET_OPCODE(ins)) {
    case OP_ENTER: 
      {
	mrb_aspec ax = GETARG_Ax(ins);
	int m1 = MRB_ASPEC_REQ(ax);
	int regno;

	/* Argument is array */
	if (pnode == NULL) {
	  /* top level */
	  for (regno = 1; regno <= m1; j++) {
	    ci->reginfo[regno].type = MRB_TT_ARRAY;
	    ci->reginfo[regno].klass = mrb->array_class;
	  }
	}
	else {
	  /* inner block */
	}
      }
      break;

    case OP_LOADI:
      {
	int regno;
	regno = GETARG_A(irep->iseq[i]);
	ci->reginfo[regno].type = MRB_TT_FIXNUM;
	ci->reginfo[regno].klass = mrb->fixnum_class;
      }
      break;

    default:
      break;
  }
}

static mrb_value
mrbjit_simd_codegen(mrb_state *mrb, mrb_irep irep, struct irep_link *pnode)
{
  int i, j;
  struct mrbjit_code_info *ci;

  for (i = 0; i < irep->ilen; i++) {
    mrb_code ins;
    irep->jit_entry_tab[i].body[0] = ci;

    ins = irep->iseq[i];

    switch (GET_OPCODE(ins)) {
    case OP_MOVE:
      if (GETARG_A(ins) < GETARG_B(ins)) {
	ci->reginfo[GETARG_A(ins)] = ci->reginfo[GETARG_B(ins)];
      }
      break;

    case OP_ENTER: 
      {
      }
      break;

    case OP_LOADI:
      {
	int regno;
	regno = GETARG_A(irep->iseq[i]);
	ci->reginfo[regno].type = MRB_TT_FIXNUM;
	ci->reginfo[regno].klass = mrb->fixnum_class;
      }
      break;

    default:
      break;
    }
  }
}

static mrb_value
mrbjit_simd_compile(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  mrb_irep *irep;
  void *code;

  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "block require");
  }
  irep = mrb_proc_ptr(blk)->body.irep;
  if (irep->jit_entry_tab == NULL) {
    mrbjit_make_jit_entry_tab(mrb, irep, irep->ilen);
  }
  mrbjit_simd_analyze(mrb, irep, NULL);

  code = mrbjit_simd_codegen(mrb, irep, NULL);

  return mrb_obj_value(Data_Wrap_Struct(mrb, self, &mrb_simdcode_type, code);
}

void
mrb_mruby_simd_gem_init(mrb_state *mrb)
{
  struct RClass *simd;

  simd = mrb_define_module(mrb, "SIMD");
  mrb_define_method(mrb, simd, "compile", mrbjit_simd_compile, MRB_ARGS_NONE());
}

void
mrb_mruby_simd_gem_final(mrb_state *mrb)
{
}

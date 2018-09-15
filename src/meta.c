#include "mruby.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/array.h"
#include "mruby/proc.h"
#include "mruby/string.h"
#include "mruby/opcode.h"
#include "mruby/irep.h"
#include "mruby/debug.h"
#if defined MRBJIT
#include "mruby/jit.h"
#endif
#include <stdio.h>

static const char *optable[] = {
  "NOP", "MOVE",
  "LOADL", "LOADI", "LOADSYM", "LOADNIL",
  "LOADSELF", "LOADT", "LOADF",
  "GETGLOBAL", "SETGLOBAL", "GETSPECIAL", "SETSPECIAL",
#if defined MRBJIT
  "GETIV", "GETIV2", "SETIV",  "SETIV2", "GETCV", "SETCV",
#else
  "GETIV", "SETIV",  "GETCV", "SETCV",
#endif
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
enum inst_type {
  N,
  A,
  Ax,
  Bx,
  AB,
  AC,
  ABx,
  AsBx,
  sBx,
  ABC,
  ASC,
  ABzCz,
};

static enum inst_type opkindtable[] = {
  N, AB,                //"NOP", "MOVE",
  ABx, AsBx, ABx, A,    //"LOADL", "LOADI", "LOADSYM", "LOADNIL",
  A, A, A,              //"LOADSELF", "LOADT", "LOADF",
  ABx, ABx, ABx, ABx,   //"GETGLOBAL", "SETGLOBAL", "GETSPECIAL", "SETSPECIAL",
#if defined MRBJIT
  ABx, ABx, ABx, ABx, ABx, ABx,   //"GETIV", "GETIV2", "SETIV", "SETIV2", " GETCV", "SETCV",
#else
  ABx, ABx, ABx, ABx,   //"GETIV", "SETIV", " GETCV", "SETCV",
#endif
  ABx, ABx, ABx, ABx,   //"GETCONST", "SETCONST", "GETMCNST", "SETMCNST",
  ABx, ABx,             //"GETUPVAR", "SETUPVAR",
  sBx, AsBx, AsBx,      //"JMP", "JMPIF", "JMPNOT",
  sBx, ABC, A, A, Bx, A,  //"ONERR", "RESCUE", "POPERR", "RAISE", "EPUSH", "EPOP",
  ASC, ASC, ASC,        //"SEND", "SENDB", "FSEND",
  A, AC, ABx, Ax,       //"CALL", "SUPER", "ARGARY", "ENTER",
  ASC, AC, AB, ASC, ABx,//"KARG", "KDICT", "RETURN", "TAILCALL", "BLKPUSH",
  ASC, ASC, ASC, ASC, ASC, ASC, //"ADD", "ADDI", "SUB", "SUBI", "MUL", "DIV",
  ASC, ASC, ASC, ASC, ASC, //"EQ", "LT", "LE", "GT", "GE",
  ABC, AB, AB, ABC, ABC, ABC, //"ARRAY", "ARYCAT", "ARYPUSH", "AREF", "ASET", "APOST",
  ABx, AB, ABC,         //"STRING", "STRCAT", "HASH",
  ABzCz, ABC, A,        //"LAMBDA", "RANGE", "OCLASS",
  AB, AB, ABx,          //"CLASS", "MODULE", "EXEC",
  AB, AB, A,            //"METHOD", "SCLASS", "TCLASS",
  ABC, N, Bx,           //"DEBUG", "STOP", "ERR",
};

static mrb_value
mrb_irep_make_optab(mrb_state *mrb)
{
  int i;
  int siz = sizeof(optable) / sizeof(optable[0]);
  int ai = mrb_gc_arena_save(mrb);
  mrb_value ary = mrb_ary_new_capa(mrb, 2);
  mrb_value opttab = mrb_ary_new_capa(mrb, siz);
  mrb_value optktab = mrb_ary_new_capa(mrb, siz);

  for (i = 0; i < siz; i++) {
    mrb_ary_push(mrb, opttab, mrb_str_new_cstr(mrb, optable[i]));
    mrb_ary_push(mrb, optktab, mrb_fixnum_value(opkindtable[i]));
  }
  
  mrb_ary_push(mrb, ary, opttab);
  mrb_ary_push(mrb, ary, optktab);
  mrb_gc_arena_restore(mrb, ai);
  return ary;
}

static void
mrb_irepobj_free(mrb_state *mrb, void *ptr)
{
  /* Do nothing */
}

static void
mrb_env_free(mrb_state *mrb, void *ptr)
{
  /* Do nothing */
}

static struct mrb_data_type mrb_irep_type = { "Irep", mrb_irepobj_free };

static struct mrb_data_type mrb_env_type = { "Env", mrb_env_free };

static mrb_value
mrb_irep_wrap(mrb_state *mrb, struct RClass *tc, struct mrb_irep *irep)
{
  mrb_irep_incref(mrb, irep);
  return mrb_obj_value(Data_Wrap_Struct(mrb, tc, &mrb_irep_type, irep));
}

static mrb_value
mrb_env_wrap(mrb_state *mrb, struct RClass *tc, struct REnv *e)
{
  return mrb_obj_value(Data_Wrap_Struct(mrb, tc, &mrb_env_type, e));
}

static mrb_value
mrb_irep_get_irep(mrb_state *mrb, mrb_value self)
{
  mrb_value recv;
  mrb_value name;
  mrb_method_t m;
  struct RClass *c;

  mrb_get_args(mrb, "oo", &recv, &name);
  c = mrb_class(mrb, recv);
  m = mrb_method_search_vm(mrb, &c, mrb_symbol(name));

  if (m.proc && MRB_METHOD_PROC_P(m)) {
    return mrb_irep_wrap(mrb, mrb_class_ptr(self), MRB_METHOD_PROC(m)->body.irep);
  }
  else {
    return mrb_nil_value();
  }
}

static mrb_value
mrb_irep_get_irep_instance(mrb_state *mrb, mrb_value self)
{
  mrb_value recv;
  mrb_value name;
  mrb_method_t m;
  struct RClass *c;

  mrb_get_args(mrb, "oo", &recv, &name);
  c = mrb_class_ptr(recv);
  m = mrb_method_search_vm(mrb, &c, mrb_symbol(name));

  if (m.proc && MRB_METHOD_PROC_P(m) && !MRB_METHOD_CFUNC_P(m)) {
    return mrb_irep_wrap(mrb, mrb_class_ptr(self), MRB_METHOD_PROC(m)->body.irep);
  }
  else {
    return mrb_nil_value();
  }
}

static mrb_value
mrb_irep_new_irep(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_value iseq;
  mrb_value pool;
  mrb_value syms;
  mrb_value nregs;
  mrb_value nlocals;
  mrb_irep *irep;

  mrb_get_args(mrb, "ooooo", &iseq, &pool, &syms, &nregs, &nlocals);
  irep = mrb_add_irep(mrb);

  irep->flags = 0;
  irep->ilen = RARRAY_LEN(iseq);
  irep->iseq = (mrb_code*)mrb_malloc(mrb, irep->ilen * sizeof(mrb_value));
  for (i = 0; i < irep->ilen; i++) {
    irep->iseq[i] = mrb_fixnum(mrb_ary_entry(iseq, i));
  }

  irep->plen = RARRAY_LEN(pool);
  irep->pool = (mrb_value *)mrb_malloc(mrb, irep->plen * sizeof(mrb_value));
  for (i = 0; i < irep->plen; i++) {
    mrb_value val = mrb_ary_entry(pool, i);

    if (mrb_type(val) == MRB_TT_STRING) {
      irep->pool[i] = mrb_str_pool(mrb, val);
    }
    else {
      irep->pool[i] = val;
    }
  }

  irep->slen = RARRAY_LEN(syms);
  irep->syms = (mrb_sym *)mrb_malloc(mrb, irep->slen * sizeof(mrb_value));
  for (i = 0; i < irep->slen; i++) {
    irep->syms[i] = mrb_symbol(mrb_ary_entry(syms, i));
  }

  irep->nregs = mrb_fixnum(nregs);
  irep->nlocals = mrb_fixnum(nlocals);
#if defined(MRBJIT)
  irep->jit_entry_tab = NULL;
  irep->method_kind = NORMAL;
  irep->simple_lambda = 0;
  irep->shared_lambda = 0;
  irep->disable_jit = 0;
  irep->block_lambda = 1;
  irep->may_overflow = 0;
  irep->proc_obj = NULL;
#endif
  return mrb_irep_wrap(mrb, mrb_class_ptr(self), irep);
}

static mrb_value
mrb_env_get_proc_env(mrb_state *mrb, mrb_value self)
{
  mrb_int level;
  mrb_value proc;
  int i;
  struct REnv *e;

  if (mrb_get_args(mrb, "o|i", &proc, &level) == 1) {
    level = 0;
  }

  e = MRB_PROC_ENV(mrb_proc_ptr(proc));

  for (i = 0; i < level; i++) {
    e = (struct REnv *)e->c;
    if (!e) return mrb_nil_value();
  }
  return mrb_env_wrap(mrb, mrb_class_ptr(self), e);
}

static mrb_value
mrb_env_get_proc_irep(mrb_state *mrb, mrb_value self)
{
  mrb_value proc;
  mrb_value oirep;
  mrb_irep *irep;

  mrb_get_args(mrb, "o", &proc);

  irep = mrb_proc_ptr(proc)->body.irep;

  oirep = mrb_irep_wrap(mrb, mrb_class_ptr(self), irep);
  mrb_write_barrier(mrb, mrb_basic_ptr(oirep));
  return oirep;
}

static mrb_value
mrb_env_get_current_env(mrb_state *mrb, mrb_value self)
{
  mrb_int level;
  struct REnv *e;
  mrb_callinfo *ci;

  if (mrb_get_args(mrb, "|i", &level) == 0) {
    level = 0;
  }
  level = level;
  ci = &mrb->c->ci[-level - 1];

  if (MRB_PROC_CFUNC_P(ci->proc)) {
    return mrb_nil_value();
  }

  if (!ci->env) {
    e = (struct REnv*)mrb_obj_alloc(mrb, MRB_TT_ENV, (struct RClass*)MRB_PROC_ENV(ci->proc));
    MRB_ENV_SET_STACK_LEN(e, ci->proc->body.irep->nregs);
    e->cxt = mrb->c;
    e->stack = mrb->c->ci[-level].stackent;
    ci->env = e;
  }
  else {
    e = ci->env;
  }

  return mrb_env_wrap(mrb, mrb_class_ptr(self), e);
}

static mrb_value
mrb_irep_id(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);

  return mrb_fixnum_value((mrb_int)irep);
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
mrb_irep_get_filename(mrb_state *mrb, mrb_value self)
{
  mrb_int pc;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_get_args(mrb, "i", &pc);

  return mrb_str_new_cstr(mrb, mrb_debug_get_filename(irep, pc));
}

static mrb_value
mrb_irep_get_line(mrb_state *mrb, mrb_value self)
{
  mrb_int pc;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_get_args(mrb, "i", &pc);

  return mrb_fixnum_value(mrb_debug_get_line(irep, pc));
}

static mrb_value
mrb_irep_set_iseq(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value src;

  mrb_get_args(mrb, "o", &src);

  if (irep->iseq && !(irep->flags & MRB_ISEQ_NO_FREE)) {
    mrb_free(mrb, irep->iseq);
  }

  irep->flags = 0;
  irep->ilen = RARRAY_LEN(src);
  irep->iseq = (mrb_code*)mrb_malloc(mrb, irep->ilen * sizeof(mrb_value));
  for (i = 0; i < irep->ilen; i++) {
    irep->iseq[i] = mrb_fixnum(mrb_ary_entry(src, i));
  }

  return src;
}

static mrb_value
mrb_irep_pool(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value ary = mrb_ary_new_capa(mrb, irep->ilen);

  
  for (i = 0; i < irep->plen; i++) {
    if (mrb_type(irep->pool[i]) == MRB_TT_STRING) {
      mrb_ary_push(mrb, ary, mrb_str_dup(mrb, irep->pool[i]));
    }
    else {
      mrb_ary_push(mrb, ary, irep->pool[i]);
    }
  }

  return ary;
}

static mrb_value
mrb_irep_set_pool(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value src;

  mrb_get_args(mrb, "o", &src);

  if (irep->pool) {
    mrb_free(mrb, irep->pool);
  }

  irep->plen = RARRAY_LEN(src);
  irep->pool = (mrb_value *)mrb_malloc(mrb, irep->plen * sizeof(mrb_value));
  for (i = 0; i < irep->plen; i++) {
    mrb_value val = mrb_ary_entry(src, i);

    if (mrb_type(val) == MRB_TT_STRING) {
      irep->pool[i] = mrb_str_pool(mrb, val);
    }
    else {
      irep->pool[i] = val;
    }
  }

  return src;
}

static mrb_value
mrb_irep_syms(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value ary = mrb_ary_new_capa(mrb, irep->ilen);

  
  for (i = 0; i < irep->slen; i++) {
    mrb_ary_push(mrb, ary, mrb_symbol_value(irep->syms[i]));
  }

  return ary;
}

static mrb_value
mrb_irep_set_syms(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value src;

  mrb_get_args(mrb, "o", &src);

  if (irep->syms) {
    mrb_free(mrb, irep->syms);
  }
    
  irep->slen = RARRAY_LEN(src);
  irep->syms = (mrb_sym *)mrb_malloc(mrb, irep->slen * sizeof(mrb_value));
  for (i = 0; i < irep->slen; i++) {
    irep->syms[i] = mrb_symbol(mrb_ary_entry(src, i));
  }

  return src;
}

static mrb_value
mrb_irep_reps(mrb_state *mrb, mrb_value self)
{
  int i;
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value ary = mrb_ary_new_capa(mrb, irep->rlen);


  for (i = 0; i < irep->rlen; i++) {
    mrb_ary_push(mrb, ary, 
		 mrb_irep_wrap(mrb, mrb_class(mrb, self), irep->reps[i]));
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
mrb_irep_set_nregs(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value src;

  mrb_get_args(mrb, "o", &src);
  irep->nregs = mrb_fixnum(src);

  return src;
}

static mrb_value
mrb_irep_nlocals(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);

  return mrb_fixnum_value((mrb_int) irep->nlocals);
}

static mrb_value
mrb_irep_set_nlocals(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrb_value src;

  mrb_get_args(mrb, "o", &src);
  irep->nlocals = mrb_fixnum(src);

  return src;
}

#if defined MRBJIT
static mrb_value
mrb_irep_regklass(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrbjit_codetab *ctab;
  mrb_value paths;
  mrb_value pc;
  int i;
  int ai = mrb_gc_arena_save(mrb);

  mrb_get_args(mrb, "o", &pc);
  ctab = &irep->jit_entry_tab[mrb_fixnum(pc)];
  paths = mrb_ary_new_capa(mrb, ctab->size);
  for (i = 0; i < ctab->size; i++) {
    mrb_value regs = mrb_ary_new_capa(mrb, irep->nregs);
    mrbjit_code_info *pent = &ctab->body[i];
    int j;

    for (j = 0; j < irep->nregs; j++) {
      mrb_value cvalue;
      if (pent->reginfo && pent->reginfo[j].klass) {
	cvalue = mrb_obj_value(pent->reginfo[j].klass);
      }
      else {
	cvalue = mrb_nil_value();
      }
      mrb_ary_push(mrb, regs, cvalue);
    }

    mrb_ary_push(mrb, paths, regs);
  }

  mrb_gc_arena_restore(mrb, ai);
  return paths;
}

static mrb_value
mrb_irep_regtype(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  mrbjit_codetab *ctab;
  mrb_value paths;
  mrb_value pc;
  int i;
  int ai = mrb_gc_arena_save(mrb);

  mrb_get_args(mrb, "o", &pc);
  ctab = &irep->jit_entry_tab[mrb_fixnum(pc)];
  paths = mrb_ary_new_capa(mrb, ctab->size);
  for (i = 0; i < ctab->size; i++) {
    mrb_value regs = mrb_ary_new_capa(mrb, irep->nregs);
    mrbjit_code_info *pent = &ctab->body[i];
    int j;

    for (j = 0; j < irep->nregs; j++) {
      mrb_value cvalue;
      if (pent->reginfo && pent->reginfo[j].type) {
	cvalue = mrb_fixnum_value(pent->reginfo[j].type);
      }
      else {
	cvalue = mrb_nil_value();
      }
      mrb_ary_push(mrb, regs, cvalue);
    }

    mrb_ary_push(mrb, paths, regs);
  }

  mrb_gc_arena_restore(mrb, ai);
  return paths;
}
#endif

static mrb_value
mrb_irep_to_proc(mrb_state *mrb, mrb_value self)
{
  mrb_irep *irep = mrb_get_datatype(mrb, self, &mrb_irep_type);
  struct RProc *p = mrb_proc_new(mrb, irep);
  struct REnv *e;

  if (!mrb->c->ci->env) {
    e = (struct REnv*)mrb_obj_alloc(mrb, MRB_TT_ENV, (struct RClass*)MRB_PROC_ENV(mrb->c->ci[-1].proc));
    e->flags= (unsigned int)mrb->c->ci[-1].proc->body.irep->nlocals;
    e->cxt = mrb->c;
    e->stack = mrb->c->ci[-1].stackent;
  }
  else {
    e = mrb->c->ci->env;
  }
  p->e.env = e;

  return mrb_obj_value(p);
}

static mrb_value
mrb_env_to_a(mrb_state *mrb, mrb_value self)
{
  struct REnv *e = mrb_get_datatype(mrb, self, &mrb_env_type);
  int len = MRB_ENV_STACK_LEN(e);
  mrb_value a = mrb_ary_new_capa(mrb, len);
  int i;

  for (i = 0; i < len; i++) {
    mrb_ary_push(mrb, a, e->stack[i]);
  }

  return a;
}

static mrb_value
mrb_meta_proc_search_proc(mrb_state *mrb, mrb_value self)
{
  mrb_value recv;
  mrb_value name;
  mrb_method_t m;
  struct RClass *c;

  mrb_get_args(mrb, "oo", &recv, &name);
  c = mrb_class_ptr(recv);
  m = mrb_method_search_vm(mrb, &c, mrb_symbol(name));

  if (m.proc && MRB_METHOD_PROC_P(m) && !MRB_METHOD_CFUNC_P(m)) {
    struct RProc *p =  MRB_METHOD_PROC(m);
    struct RProc *p2 = (struct RProc*)mrb_obj_alloc(mrb, MRB_TT_PROC, mrb->proc_class);
    mrb_proc_copy(p2, p);
    p2->e.target_class = p->e.target_class;
    p2->flags |= MRB_PROC_STRICT;
    return mrb_obj_value(p2);
  }
  else {
    return mrb_nil_value();
  }
}

static mrb_value
mrb_meta_proc_target_class(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);

  return mrb_obj_value(MRB_PROC_TARGET_CLASS(p));
}

static mrb_value
mrb_meta_proc_upper(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);

  if (p->upper) {
    struct RProc *p2 = (struct RProc*)mrb_obj_alloc(mrb, MRB_TT_PROC, mrb->proc_class);
    mrb_proc_copy(p2, p->upper);
    p2->e.target_class = p->upper->e.target_class;
    p2->flags |= MRB_PROC_STRICT;
    return mrb_obj_value(p2);
  }
  else {
    return mrb_nil_value();
  }
}

void
mrb_mruby_meta_circular_gem_init(mrb_state *mrb)
{
  struct RClass *a;

  a = mrb_define_class(mrb, "Irep", mrb->object_class);
  MRB_SET_INSTANCE_TT(a, MRB_TT_DATA);

  mrb_define_const(mrb, a, "OPTABLE", mrb_irep_make_optab(mrb));
  mrb_define_class_method(mrb, a, "get_irep", mrb_irep_get_irep, MRB_ARGS_REQ(2));
  mrb_define_class_method(mrb, a, "get_irep_instance", mrb_irep_get_irep_instance, MRB_ARGS_REQ(2));
  mrb_define_class_method(mrb, a, "new_irep", mrb_irep_new_irep, MRB_ARGS_REQ(5));

  mrb_define_method(mrb, a, "id", mrb_irep_id, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "iseq", mrb_irep_iseq, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "iseq=", mrb_irep_set_iseq, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "pool", mrb_irep_pool, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "pool=", mrb_irep_set_pool, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "syms", mrb_irep_syms, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "syms=", mrb_irep_set_syms, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "reps", mrb_irep_reps, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "nregs", mrb_irep_nregs, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "nregs=", mrb_irep_set_nregs, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "nlocals", mrb_irep_nlocals, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "nlocals=", mrb_irep_set_nlocals, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "filename", mrb_irep_get_filename, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "line", mrb_irep_get_line, MRB_ARGS_REQ(1));
#if defined MRBJIT
  mrb_define_method(mrb, a, "reg_class", mrb_irep_regklass, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "reg_type", mrb_irep_regtype, MRB_ARGS_REQ(1));
#endif
  mrb_define_method(mrb, a, "to_proc", mrb_irep_to_proc, MRB_ARGS_NONE());

  mrb_define_class_method(mrb, a, "get_proc_irep", mrb_env_get_proc_irep, MRB_ARGS_REQ(1));

  a = mrb_define_class(mrb, "Env", mrb->object_class);
  MRB_SET_INSTANCE_TT(a, MRB_TT_DATA);
  mrb_define_class_method(mrb, a, "get_proc_env", mrb_env_get_proc_env, MRB_ARGS_ANY());
  mrb_define_class_method(mrb, a, "get_current_env", mrb_env_get_current_env, MRB_ARGS_ANY());
  mrb_define_method(mrb, a, "to_a", mrb_env_to_a, MRB_ARGS_NONE());

  a = mrb->proc_class;
  mrb_define_class_method(mrb, a, "search_proc", mrb_meta_proc_search_proc, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, a, "upper", mrb_meta_proc_upper, MRB_ARGS_NONE());
  mrb_define_method(mrb, a, "target_class", mrb_meta_proc_target_class, MRB_ARGS_NONE());
}

void
mrb_mruby_meta_circular_gem_final(mrb_state *mrb)
{
}

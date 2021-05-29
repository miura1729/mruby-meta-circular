/*
** vm.c - virtual machine for mruby
**
** See Copyright Notice in mruby.h
*/

#include <stddef.h>
#include <stdarg.h>
#include <math.h>
#include <assert.h>
#include <mruby.h>
#include <mruby/jit.h>
#include <mruby/irep.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/hash.h>
#include <mruby/irep.h>
#include <mruby/numeric.h>
#include <mruby/proc.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/opcode.h>
#include "value_array.h"
#include <mruby/throw.h>

#ifdef MRB_DISABLE_STDIO
#if defined(__cplusplus)
extern "C" {
#endif
void abort(void);
#if defined(__cplusplus)
}  /* extern "C" { */
#endif
#endif

#define STACK_INIT_SIZE 128
#define CALLINFO_INIT_SIZE 32

#ifndef ENSURE_STACK_INIT_SIZE
#define ENSURE_STACK_INIT_SIZE 16
#endif

#ifndef RESCUE_STACK_INIT_SIZE
#define RESCUE_STACK_INIT_SIZE 16
#endif

/* Define amount of linear stack growth. */
#ifndef MRB_STACK_GROWTH
#define MRB_STACK_GROWTH 128
#endif

/* Maximum mrb_funcall() depth. Should be set lower on memory constrained systems. */
#ifndef MRB_FUNCALL_DEPTH_MAX
#define MRB_FUNCALL_DEPTH_MAX 512
#endif

/* Maximum depth of ecall() recursion. */
#ifndef MRB_ECALL_DEPTH_MAX
#define MRB_ECALL_DEPTH_MAX 32
#endif

/* Maximum stack depth. Should be set lower on memory constrained systems.
The value below allows about 60000 recursive calls in the simplest case. */
#ifndef MRB_STACK_MAX
#define MRB_STACK_MAX (0x40000 - MRB_STACK_GROWTH)
#endif

#ifdef VM_DEBUG
# define DEBUG(x) (x)
#else
# define DEBUG(x)
#endif


#ifndef MRB_GC_FIXED_ARENA
static void
mrb_gc_arena_shrink(mrb_state *mrb, int idx)
{
  mrb_gc *gc = &mrb->gc;
  int capa = gc->arena_capa;

  if (idx < capa / 4) {
    capa >>= 2;
    if (capa < MRB_GC_ARENA_SIZE) {
      capa = MRB_GC_ARENA_SIZE;
    }
    if (capa != gc->arena_capa) {
      gc->arena = (struct RBasic**)mrb_realloc(mrb, gc->arena, sizeof(struct RBasic*)*capa);
      gc->arena_capa = capa;
    }
  }
}
#else
#define mrb_gc_arena_shrink(mrb,idx)
#endif

#define CALL_MAXARGS 127

void mrb_method_missing(mrb_state *mrb, mrb_sym name, mrb_value self, mrb_value args);

static inline void
stack_clear(mrb_state *mrb, mrb_value *from, size_t count)
{
  assert(count < 10000);
#ifndef MRB_NAN_BOXING
  const mrb_value mrb_value_zero = { { 0 } };

  while (count-- > 0) {
    *from++ = mrb_value_zero;
  }
#else
  while (count-- > 0) {
    SET_NIL_VALUE(*from);
    from++;
  }
#endif
}

void
mrbjit_stack_clear(mrb_state *mrb, mrb_value *from, size_t count)
{
  assert(count < 10000);
#ifndef MRB_NAN_BOXING
  const mrb_value mrb_value_zero = { { 0 } };

  while (count-- > 0) {
    *from++ = mrb_value_zero;
  }
#else
  while (count-- > 0) {
    SET_NIL_VALUE(*from);
    from++;
  }
#endif
}

static inline void
stack_copy(mrb_value *dst, const mrb_value *src, size_t size)
{
  while (size-- > 0) {
    *dst++ = *src++;
  }
}

static void
stack_init(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;

  /* mrb_assert(mrb->stack == NULL); */
  c->stbase = (mrb_value *)mrb_calloc(mrb, STACK_INIT_SIZE, sizeof(mrb_value));
  c->stend = c->stbase + STACK_INIT_SIZE;
  c->stack = c->stbase;

  /* mrb_assert(ci == NULL); */
  c->cibase_org = (mrb_callinfo *)mrb_calloc(mrb, CALLINFO_INIT_SIZE + 2, sizeof(mrb_callinfo));
  c->cibase = (mrb_callinfo *)((((uintptr_t)(c->cibase_org)) & (~(64 - 1))) + 64);
  c->ciend = c->cibase + CALLINFO_INIT_SIZE;
  c->ci = c->cibase;
  c->ci->target_class = mrb->object_class;
  c->ci->stackent = c->stack;
}

static inline void
envadjust(mrb_state *mrb, mrb_value *oldbase, mrb_value *newbase, size_t size)
{
  mrb_callinfo *ci = mrb->c->cibase;
  ptrdiff_t off;

  if (newbase == oldbase) return;
  off = newbase - oldbase;
  while (ci <= mrb->c->ci) {
    struct REnv *e = ci->env;
    mrb_value *st;

    if (e && MRB_ENV_STACK_SHARED_P(e) &&
        (st = e->stack) && oldbase <= st && st < oldbase+size) {
      ptrdiff_t off = e->stack - oldbase;

      e->stack = newbase + off;
    }
    ci->stackent += off;
    ci++;
  }
}

/** def rec ; $deep =+ 1 ; if $deep > 1000 ; return 0 ; end ; rec ; end  */

static void
stack_extend_alloc(mrb_state *mrb, int room)
{
  mrb_value *oldbase = mrb->c->stbase;
  mrb_value *newstack;
  size_t oldsize = mrb->c->stend - mrb->c->stbase;
  size_t size = oldsize;
  size_t off = mrb->c->stack - mrb->c->stbase;

  if (off > size) size = off;
#ifdef MRB_STACK_EXTEND_DOUBLING
  if (room <= size)
    size *= 2;
  else
    size += room;
#else
  /* Use linear stack growth.
     It is slightly slower than doubling the stack space,
     but it saves memory on small devices. */
  if (room <= MRB_STACK_GROWTH)
    size += MRB_STACK_GROWTH;
  else
    size += room;
#endif

  newstack = (mrb_value *)mrb_realloc(mrb, mrb->c->stbase, sizeof(mrb_value) * size);
  if (newstack == NULL) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
  }
  stack_clear(mrb, &(newstack[oldsize]), size - oldsize);
  envadjust(mrb, oldbase, newstack, oldsize);
  mrb->c->stbase = newstack;
  mrb->c->stack = mrb->c->stbase + off;
  mrb->c->stend = mrb->c->stbase + size;

  /* Raise an exception if the new stack size will be too large,
     to prevent infinite recursion. However, do this only after resizing the stack, so mrb_raise has stack space to work with. */
  if (size > MRB_STACK_MAX) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
  }
}

static inline void
stack_extend(mrb_state *mrb, int room)
{
  if (mrb->c->stack + room >= mrb->c->stend) {
    stack_extend_alloc(mrb, room);
  }
}

void
mrbjit_stack_extend(mrb_state *mrb, int room)
{
  if (mrb->c->stack + room >= mrb->c->stend) {
    stack_extend_alloc(mrb, room);
  }
}

static inline struct REnv*
uvenv(mrb_state *mrb, int up)
{
  struct RProc *proc = mrb->c->ci->proc;
  struct REnv *e;

  while (up--) {
    proc = proc->upper;
    if (!proc) return NULL;
  }
  e = MRB_PROC_ENV(proc);
  if (e) return e;              /* proc has enclosed env */
  else {
    mrb_callinfo *ci = mrb->c->ci;
    mrb_callinfo *cb = mrb->c->cibase;

    while (cb <= ci) {
      if (ci->proc->body.irep->org_iseq == proc->body.irep->org_iseq) {
        return ci->env;
      }
      ci--;
    }
  }
  return NULL;
}

int
mrbjit_uvoff(mrb_state *mrb, int up)
{
  int off = 0;
  struct RProc *proc = mrb->c->ci->proc;
  struct REnv *e;
  mrb_callinfo *ci = mrb->c->ci;
  mrb_callinfo *cb = mrb->c->cibase;

  while (up--) {
    proc = proc->upper;
    if (!proc) return 0;
  }
  e = MRB_PROC_ENV(proc);
  if (e) return -2;              /* proc has enclosed env */

  while (cb <= ci) {
    if (ci->proc->body.irep->org_iseq == proc->body.irep->org_iseq) {
      return off;
    }
    ci--;
    off++;
  }

  return 0;
}

static inline struct RProc*
top_proc(mrb_state *mrb, struct RProc *proc)
{
  while (proc->upper) {
    if (MRB_PROC_SCOPE_P(proc) || MRB_PROC_STRICT_P(proc))
      return proc;
    proc = proc->upper;
  }
  return proc;
}

struct RProc*
mrbjit_top_proc(mrb_state *mrb, struct RProc *proc)
{
  while (proc->upper) {
    if (MRB_PROC_SCOPE_P(proc) || MRB_PROC_STRICT_P(proc))
      return proc;
    proc = proc->upper;
  }
  return proc;
}

static void
alloc_proc_pool(mrb_state *mrb, size_t size)
{
  struct mrb_context *c = mrb->c;
  int i;
  struct LocalProc *lp;
  struct LocalProc *newpp;
  newpp = (struct LocalProc *)
    mrb_calloc(mrb, size + 1, sizeof(struct LocalProc));
  if (c->proc_pool) {
    c->proc_pool->proc.c = (struct RClass*)newpp;
  }
  else {
    c->proc_pool_base = c->proc_pool = newpp;
  }
  for (i = 0, lp = newpp; i < size; i++, lp++) {
    lp->proc.tt = MRB_TT_PROC;
    lp->proc.c = mrb->proc_class;
    lp->proc.e.env = &lp->env;
    lp->env.tt = MRB_TT_ENV;
  }
  
  lp->proc.tt = MRB_TT_FREE;
  lp->proc.c = NULL;
}

static struct RProc *
get_local_proc(mrb_state *mrb, mrb_irep *mirep)
{
  struct mrb_context *c = mrb->c;
  struct RProc *p;
  int i;

  if (c->proc_pool == NULL) {
    if (c->proc_pool_base) {
      c->proc_pool = c->proc_pool_base;
    }
    else {
      c->proc_pool_capa = 3;
      alloc_proc_pool(mrb, c->proc_pool_capa);
    }
  }

  /* Search already allocated proc object */
  /* This function for unescaped lambda so don't warry */
  for (i = -1; c->proc_pool[i].proc.tt == MRB_TT_PROC; i--) {
    if (c->proc_pool[i].proc.body.irep == mirep) {
      p = &(c->proc_pool[i].proc);
      return p;
    }
  }

  if (c->proc_pool->proc.tt == MRB_TT_FREE) {
    if (c->proc_pool->proc.c == NULL) {
      c->proc_pool_capa = c->proc_pool_capa * 2;
      alloc_proc_pool(mrb, c->proc_pool_capa);
    }

    c->proc_pool = (struct LocalProc *)c->proc_pool->proc.c;
  }

  p = &(c->proc_pool->proc);
  p->e.env->stack = mrb->c->stack;
  p->e.env->c = (struct RClass*)mrb->c->ci->proc->e.env;
  p->flags = 0;
  c->proc_pool->proc.body.irep = mirep;
  mirep->block_lambda = 1;
  mirep->flags |= MRB_ISEQ_NO_FREE; /* Guard from gc  */
  mrb_irep_incref(mrb, mirep);
  p->color = mrb->gc.current_white_part;
  p->e.env->color = mrb->gc.current_white_part;
  
  c->proc_pool++;
  return p;
}

struct RProc *
mrbjit_get_local_proc(mrb_state *mrb, mrb_irep *mirep)
{
  return get_local_proc(mrb, mirep);
}
  
#define CI_ACC_SKIP    -1
#define CI_ACC_DIRECT  -2
#define CI_ACC_RESUMED -3

static inline mrb_callinfo*
cipush(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci;

  int ridx = ci->ridx;

  if (ci + 1 == c->ciend) {
    ptrdiff_t size = ci - c->cibase;
    mrb_callinfo *sci;
    mrb_callinfo *dci;

    c->cibase_org = (mrb_callinfo *)mrb_malloc(mrb, sizeof(mrb_callinfo)*size*2 + 64);
    sci = c->cibase;
    c->cibase = (mrb_callinfo *)(((uintptr_t)(c->cibase_org) + 63) & (~(64 - 1)));
    for (dci = c->cibase; sci <= c->ci; sci++, dci++) {
      *dci = *sci;
    }

    c->ci = c->cibase + size;
    c->ciend = c->cibase + size * 2;
  }
  c->ci->jit_entry = NULL;
  ci = ++c->ci;
  ci->epos = mrb->c->eidx;
  ci->ridx = ridx;
  ci->env = 0;
  ci->pc = 0;
  ci->err = 0;
  ci->proc = 0;
  ci->acc = 0;
  ci->method_arg_ver = 0;
  ci->prev_pc = NULL;

  return ci;
}

mrb_callinfo*
mrbjit_cipush(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci;

  int ridx = ci->ridx;

  if (ci + 1 == c->ciend) {
    ptrdiff_t size = ci - c->cibase;
    mrb_callinfo *sci;
    mrb_callinfo *dci;

    c->cibase_org = (mrb_callinfo *)mrb_malloc(mrb, sizeof(mrb_callinfo)*size*2 + 64);
    sci = c->cibase;
    c->cibase = (mrb_callinfo *)(((uintptr_t)(c->cibase_org) + 63) & (~(64 - 1)));
    for (dci = c->cibase; sci <= c->ci; sci++, dci++) {
      *dci = *sci;
    }

    c->ci = c->cibase + size;
    c->ciend = c->cibase + size * 2;
  }
  c->ci->jit_entry = NULL;
  ci = ++c->ci;
  ci->epos = mrb->c->eidx;
  ci->ridx = ridx;
  ci->env = 0;
  ci->pc = 0;
  ci->err = 0;
  ci->proc = 0;
  ci->acc = 0;
  ci->method_arg_ver = 0;
  ci->prev_pc = NULL;
  ci->acc = 0;

  return ci;
}

void
mrb_env_unshare(mrb_state *mrb, struct REnv *e)
{
  if (e == NULL) return;
  else {
    size_t len = (size_t)MRB_ENV_STACK_LEN(e);
    mrb_value *p;

    if (!MRB_ENV_STACK_SHARED_P(e)) return;
    if (e->cxt != mrb->c) return;
    p = (mrb_value *)mrb_malloc(mrb, sizeof(mrb_value)*len);
    if (len > 0) {
      stack_copy(p, e->stack, len);
    }
    e->stack = p;
    MRB_ENV_UNSHARE_STACK(e);
    mrb_write_barrier(mrb, (struct RBasic *)e);
  }
}

static inline void
cipop(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;
  struct REnv *env = c->ci->env;

  c->ci--;
  if (env) {
    mrb_env_unshare(mrb, env);
  }
  if (env) mrb_env_unshare(mrb, env);
}

void
mrbjit_cipop(mrb_state *mrb)
{
  cipop(mrb);
}

void mrb_exc_set(mrb_state *mrb, mrb_value exc);

static void
ecall(mrb_state *mrb)
{
  struct RProc *p;
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci;
  struct RObject *exc;
  int orgdisflg = mrb->compile_info.disable_jit;
  ptrdiff_t cioff;
  int ai = mrb_gc_arena_save(mrb);
  int i = --c->eidx;
  int nregs;

  if (i<0) return;
  if (ci - c->cibase > MRB_ECALL_DEPTH_MAX) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
  }
  p = c->ensure[i];
  if (!p) return;
  mrb_assert(!MRB_PROC_CFUNC_P(p));
  c->ensure[i] = NULL;
  nregs = p->upper->body.irep->nregs;
  if (ci->proc && !MRB_PROC_CFUNC_P(ci->proc) &&
      ci->proc->body.irep->nregs > nregs) {
    nregs = ci->proc->body.irep->nregs;
  }
  cioff = ci - c->cibase;
  ci = cipush(mrb);
  ci->stackent = mrb->c->stack;
  ci->mid = ci[-1].mid;
  ci->acc = CI_ACC_SKIP;
  ci->argc = 0;
  ci->proc = p;
  ci->nregs = p->body.irep->nregs;
  ci->target_class = MRB_PROC_TARGET_CLASS(p);
  c->stack += nregs;
  exc = mrb->exc; mrb->exc = 0;
  mrb->compile_info.disable_jit = 1;
  if (exc) {
    mrb_gc_protect(mrb, mrb_obj_value(exc));
  }

  mrb_run(mrb, p, c->stack[0]);
  mrb->c = c;
  mrb->compile_info.disable_jit = orgdisflg;
  mrb->c->ensure[i] = NULL;
  c->ci = c->cibase + cioff;

  if (!mrb->exc) mrb->exc = exc;
  mrb_gc_arena_restore(mrb, ai);
}

void
mrbjit_ecall(mrb_state *mrb)
{
  ecall(mrb);
}

#ifndef MRB_FUNCALL_ARGC_MAX
#define MRB_FUNCALL_ARGC_MAX 16
#endif

MRB_API mrb_value
mrb_funcall(mrb_state *mrb, mrb_value self, const char *name, mrb_int argc, ...)
{
  mrb_value argv[MRB_FUNCALL_ARGC_MAX];
  va_list ap;
  mrb_int i;
  mrb_sym mid = mrb_intern_cstr(mrb, name);

  if (argc > MRB_FUNCALL_ARGC_MAX) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Too long arguments. (limit=" MRB_STRINGIZE(MRB_FUNCALL_ARGC_MAX) ")");
  }

  va_start(ap, argc);
  for (i = 0; i < argc; i++) {
    argv[i] = va_arg(ap, mrb_value);
  }
  va_end(ap);
  return mrb_funcall_argv(mrb, self, mid, argc, argv);
}

MRB_API mrb_value
mrb_funcall_with_block(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, const mrb_value *argv, mrb_value blk)
{
  mrb_value val;

  if (!mrb->jmp) {
    struct mrb_jmpbuf c_jmp;
    ptrdiff_t nth_ci = mrb->c->ci - mrb->c->cibase;

    MRB_TRY(&c_jmp) {
      mrb->jmp = &c_jmp;
      /* recursive call */
      val = mrb_funcall_with_block(mrb, self, mid, argc, argv, blk);
      mrb->jmp = 0;
    }
    MRB_CATCH(&c_jmp) { /* error */
      while (nth_ci < (mrb->c->ci - mrb->c->cibase)) {
        mrb->c->stack = mrb->c->ci->stackent;
        cipop(mrb);
      }
      mrb->jmp = 0;
      val = mrb_obj_value(mrb->exc);
    }
    MRB_END_EXC(&c_jmp);
    mrb->jmp = 0;
  }
  else {
    mrb_method_t m;
    struct RClass *c;
    mrb_callinfo *ci;
    int n;
    ptrdiff_t voff = -1;

    if (!mrb->c->stack) {
      stack_init(mrb);
    }
    n = mrb->c->ci->nregs;
    if (argc < 0) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative argc for funcall (%S)", mrb_fixnum_value(argc));
    }
    c = mrb_class(mrb, self);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (MRB_METHOD_UNDEF_P(m)) {
      mrb_sym missing = mrb_intern_lit(mrb, "method_missing");
      mrb_value args = mrb_ary_new_from_values(mrb, argc, argv);
      m = mrb_method_search_vm(mrb, &c, missing);
      if (MRB_METHOD_UNDEF_P(m)) {
        mrb_method_missing(mrb, mid, self, args);
      }
      mrb_ary_unshift(mrb, args, mrb_symbol_value(mid));
      stack_extend(mrb, n+2);
      mrb->c->stack[n+1] = args;
      argc = -1;
    }
    if (mrb->c->ci - mrb->c->cibase > MRB_FUNCALL_DEPTH_MAX) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
    }
    if (MRB_DMETHOD_CFUNC_P(m)) {
      int ai = mrb_gc_arena_save(mrb);
      mrb->c->stack[n] = self;
      if (argc > 0) {
	stack_copy(mrb->c->stack+n+1, argv, argc);
      }
      mrb->c->stack[argc+n+1] = blk;
      val = MRB_DMETHOD_CFUNC(m)(mrb, mrb->c->stack + n, argc);
      mrb_gc_arena_restore(mrb, ai);
      return val;
    }
    ci = cipush(mrb);
    ci->mid = mid;
    ci->stackent = mrb->c->stack;
    ci->argc = (int)argc;
    ci->target_class = c;
    mrb->c->stack = mrb->c->stack + n;
    if (mrb->c->stbase <= argv && argv < mrb->c->stend) {
      voff = argv - mrb->c->stbase;
    }
    if (MRB_METHOD_CFUNC_P(m)) {
      ci->nregs = (int)(argc + 2);
      stack_extend(mrb, ci->nregs);
    }
    else if (argc >= CALL_MAXARGS) {
      mrb_value args = mrb_ary_new_from_values(mrb, argc, argv);
      stack_extend(mrb, ci->nregs+2);
      mrb->c->stack[1] = args;
      ci->argc = -1;
      argc = 1;
    }
    else {
      struct RProc *p = MRB_METHOD_PROC(m);
      ci->proc = p;
      if (argc < 0) argc = 1;
      ci->nregs = (int)(p->body.irep->nregs + argc);
      stack_extend(mrb, ci->nregs);
    }
    if (voff >= 0) {
      argv = mrb->c->stbase + voff;
    }
    mrb->c->stack[0] = self;
    if (ci->argc > 0) {
      stack_copy(mrb->c->stack+1, argv, argc);
    }
    mrb->c->stack[argc+1] = blk;

    if (MRB_METHOD_CFUNC_P(m)) {
      int ai = mrb_gc_arena_save(mrb);

      ci->acc = CI_ACC_DIRECT;
      if (MRB_METHOD_PROC_P(m)) {
        ci->proc = MRB_METHOD_PROC(m);
      }
      val = MRB_METHOD_CFUNC(m)(mrb, self);
      mrb->c->stack = mrb->c->ci->stackent;
      cipop(mrb);
      mrb_gc_arena_restore(mrb, ai);
    }
    else {
      ci->acc = CI_ACC_SKIP;
      val = mrb_run(mrb, MRB_METHOD_PROC(m), self);
    }
  }
  mrb_gc_protect(mrb, val);
  return val;
}

MRB_API mrb_value
mrb_funcall_argv(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, const mrb_value *argv)
{
  int orgdisflg = mrb->compile_info.disable_jit;
  mrb_value rc;
  mrb->compile_info.disable_jit = 1;
  rc = mrb_funcall_with_block(mrb, self, mid, argc, argv, mrb_nil_value());
  mrb->compile_info.disable_jit = orgdisflg;

  return rc;
}

mrb_value
mrb_exec_irep(mrb_state *mrb, mrb_value self, struct RProc *p)
{
  mrb_callinfo *ci = mrb->c->ci;
  int keep;

  mrb->c->stack[0] = self;
  ci->proc = p;
  if (MRB_PROC_CFUNC_P(p)) {
    return MRB_PROC_CFUNC(p)(mrb, self);
  }
  ci->nregs = p->body.irep->nregs;
  if (ci->argc < 0) keep = 3;
  else keep = ci->argc + 1;
  if (ci->nregs < keep) {
    stack_extend(mrb, keep);
  }
  else {
    stack_extend(mrb, ci->nregs);
    stack_clear(mrb, mrb->c->stack+keep, ci->nregs-keep);
  }

  ci = cipush(mrb);
  ci->nregs = 0;
  ci->target_class = 0;
  ci->pc = p->body.irep->iseq;
  ci->stackent = mrb->c->stack;
  ci->acc = 0;

  return self;
}

/* 15.3.1.3.4  */
/* 15.3.1.3.44 */
/*
 *  call-seq:
 *     obj.send(symbol [, args...])        -> obj
 *     obj.__send__(symbol [, args...])      -> obj
 *
 *  Invokes the method identified by _symbol_, passing it any
 *  arguments specified. You can use <code>__send__</code> if the name
 *  +send+ clashes with an existing method in _obj_.
 *
 *     class Klass
 *       def hello(*args)
 *         "Hello " + args.join(' ')
 *       end
 *     end
 *     k = Klass.new
 *     k.send :hello, "gentle", "readers"   #=> "Hello gentle readers"
 */
MRB_API mrb_value
mrb_f_send(mrb_state *mrb, mrb_value self)
{
  mrb_sym name;
  mrb_value block, *argv, *regs;
  mrb_int argc, i, len;
  mrb_method_t m;
  struct RClass *c;
  mrb_callinfo *ci;

  mrb_get_args(mrb, "n*&", &name, &argv, &argc, &block);
  ci = mrb->c->ci;
  if (ci->acc < 0) {
  funcall:
    return mrb_funcall_with_block(mrb, self, name, argc, argv, block);
  }

  c = mrb_class(mrb, self);
  m = mrb_method_search_vm(mrb, &c, name);
  if (MRB_METHOD_UNDEF_P(m)) {            /* call method_mising */
    goto funcall;
  }

  ci->mid = name;
  ci->target_class = c;
  regs = mrb->c->stack+1;
  /* remove first symbol from arguments */
  if (ci->argc >= 0) {
    for (i=0,len=ci->argc; i<len; i++) {
      regs[i] = regs[i+1];
    }
    ci->argc--;
  }
  else {                     /* variable length arguments */
    mrb_ary_shift(mrb, regs[0]);
  }

  if (MRB_METHOD_CFUNC_P(m)) {
    if (MRB_METHOD_PROC_P(m)) {
      ci->proc = MRB_METHOD_PROC(m);
    }
    return MRB_METHOD_CFUNC(m)(mrb, self);
  }
  else if (MRB_DMETHOD_CFUNC_P(m)) {
    return MRB_DMETHOD_CFUNC(m)(mrb, regs, ci->argc);
  }
  return mrb_exec_irep(mrb, self, MRB_METHOD_PROC(m));
}

static mrb_value
eval_under(mrb_state *mrb, mrb_value self, mrb_value blk, struct RClass *c)
{
  struct RProc *p;
  mrb_callinfo *ci;

  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  ci = mrb->c->ci;
  if (ci->acc == CI_ACC_DIRECT) {
    ci->target_class = c;
    return mrb_yield_cont(mrb, blk, self, 1, &self);
  }
  ci->target_class = c;
  p = mrb_proc_ptr(blk);
  ci->proc = p;
  ci->argc = 1;
  ci->mid = ci[-1].mid;
  if (MRB_PROC_CFUNC_P(p)) {
    stack_extend(mrb, 3);
    mrb->c->stack[0] = self;
    mrb->c->stack[1] = self;
    mrb->c->stack[2] = mrb_nil_value();
    return MRB_PROC_CFUNC(p)(mrb, self);
  }
  ci->nregs = p->body.irep->nregs;
  stack_extend(mrb, (ci->nregs < 3) ? 3 : ci->nregs);
  mrb->c->stack[0] = self;
  mrb->c->stack[1] = self;
  mrb->c->stack[2] = mrb_nil_value();
  ci = cipush(mrb);
  ci->nregs = 0;
  ci->target_class = 0;
  ci->pc = p->body.irep->iseq;
  ci->stackent = mrb->c->stack;
  ci->acc = 0;

  return self;
}

/* 15.2.2.4.35 */
/*
 *  call-seq:
 *     mod.class_eval {| | block }  -> obj
 *     mod.module_eval {| | block } -> obj
 *
 *  Evaluates block in the context of _mod_. This can
 *  be used to add methods to a class. <code>module_eval</code> returns
 *  the result of evaluating its argument.
 */
mrb_value
mrb_mod_module_eval(mrb_state *mrb, mrb_value mod)
{
  mrb_value a, b;

  if (mrb_get_args(mrb, "|S&", &a, &b) == 1) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "module_eval/class_eval with string not implemented");
  }
  return eval_under(mrb, mod, b, mrb_class_ptr(mod));
}

/* 15.3.1.3.18 */
/*
 *  call-seq:
 *     obj.instance_eval {| | block }                       -> obj
 *
 *  Evaluates the given block,within  the context of the receiver (_obj_).
 *  In order to set the context, the variable +self+ is set to _obj_ while
 *  the code is executing, giving the code access to _obj_'s
 *  instance variables. In the version of <code>instance_eval</code>
 *  that takes a +String+, the optional second and third
 *  parameters supply a filename and starting line number that are used
 *  when reporting compilation errors.
 *
 *     class KlassWithSecret
 *       def initialize
 *         @secret = 99
 *       end
 *     end
 *     k = KlassWithSecret.new
 *     k.instance_eval { @secret }   #=> 99
 */
mrb_value
mrb_obj_instance_eval(mrb_state *mrb, mrb_value self)
{
  mrb_value a, b;
  mrb_value cv;
  struct RClass *c;

  if (mrb_get_args(mrb, "|S&", &a, &b) == 1) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "instance_eval with string not implemented");
  }
  switch (mrb_type(self)) {
  case MRB_TT_SYMBOL:
  case MRB_TT_FIXNUM:
#ifndef MRB_WITHOUT_FLOAT
  case MRB_TT_FLOAT:
#endif
    c = 0;
    break;
  default:
    //cv = mrb_singleton_class(mrb, self);
    cv = self;
    c = mrb_class_ptr(cv);
    break;
  }
  return eval_under(mrb, self, b, c);
}

MRB_API mrb_value
mrb_yield_with_class(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value self, struct RClass *c)
{
  struct RProc *p;
  mrb_sym mid = mrb->c->ci->mid;
  mrb_callinfo *ci;
  int n = mrb->c->ci->nregs;
  mrb_value val;

  if (mrb_nil_p(b)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  if (mrb->c->ci - mrb->c->cibase > MRB_FUNCALL_DEPTH_MAX) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
  }
  p = mrb_proc_ptr(b);
  ci = cipush(mrb);
  ci->mid = mid;
  ci->proc = p;
  ci->stackent = mrb->c->stack;
  ci->argc = (int)argc;
  ci->target_class = c;
  ci->acc = CI_ACC_SKIP;
  mrb->c->stack = mrb->c->stack + n;
  ci->nregs = MRB_PROC_CFUNC_P(p) ? (int)(argc+2) : p->body.irep->nregs;
  stack_extend(mrb, ci->nregs);

  mrb->c->stack[0] = self;
  if (argc > 0) {
    stack_copy(mrb->c->stack+1, argv, argc);
  }
  mrb->c->stack[argc+1] = mrb_nil_value();

  if (MRB_PROC_CFUNC_P(p)) {
    int orgdisflg = mrb->compile_info.disable_jit;

    mrb->compile_info.disable_jit = 1;
    val = MRB_PROC_CFUNC(p)(mrb, self);
    mrb->compile_info.disable_jit = orgdisflg;

    mrb->c->stack = mrb->c->ci->stackent;
    cipop(mrb);
  }
  else {
    val = mrb_run(mrb, p, self);
  }
  return val;
}

MRB_API mrb_value
mrb_yield_argv(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv)
{
  struct RProc *p = mrb_proc_ptr(b);

  return mrb_yield_with_class(mrb, b, argc, argv, MRB_PROC_ENV(p)->stack[0], MRB_PROC_TARGET_CLASS(p));
}

MRB_API mrb_value
mrb_yield(mrb_state *mrb, mrb_value b, mrb_value arg)
{
  struct RProc *p = mrb_proc_ptr(b);

  return mrb_yield_with_class(mrb, b, 1, &arg, MRB_PROC_ENV(p)->stack[0], MRB_PROC_TARGET_CLASS(p));
}

mrb_value
mrb_yield_cont(mrb_state *mrb, mrb_value b, mrb_value self, mrb_int argc, const mrb_value *argv)
{
  struct RProc *p;
  mrb_callinfo *ci;

  if (mrb_nil_p(b)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  if (mrb_type(b) != MRB_TT_PROC) {
    mrb_raise(mrb, E_TYPE_ERROR, "not a block");
  }

  p = mrb_proc_ptr(b);
  ci = mrb->c->ci;

  stack_extend(mrb, 3);
  mrb->c->stack[1] = mrb_ary_new_from_values(mrb, argc, argv);
  mrb->c->stack[2] = mrb_nil_value();
  ci->argc = -1;
  return mrb_exec_irep(mrb, self, p);
}

mrb_value
mrb_mod_s_nesting(mrb_state *mrb, mrb_value mod)
{
  struct RProc *proc;
  mrb_value ary;
  struct RClass *c = NULL;

  mrb_get_args(mrb, "");
  ary = mrb_ary_new(mrb);
  proc = mrb->c->ci[-1].proc;   /* callee proc */
  mrb_assert(!MRB_PROC_CFUNC_P(proc));
  while (proc) {
    if (MRB_PROC_SCOPE_P(proc)) {
      struct RClass *c2 = MRB_PROC_TARGET_CLASS(proc);

      if (c2 != c) {
        c = c2;
        mrb_ary_push(mrb, ary, mrb_obj_value(c));
      }
    }
    proc = proc->upper;
  }
  return ary;
}

static struct RBreak*
break_new(mrb_state *mrb, struct RProc *p, mrb_value val)
{
  struct RBreak *brk;

  brk = (struct RBreak*)mrb_obj_alloc(mrb, MRB_TT_BREAK, NULL);
  brk->proc = p;
  brk->val = val;

  return brk;
}

struct RBreak*
mrbjit_break_new(mrb_state *mrb, struct RProc *p, mrb_value val)
{
  return break_new(mrb, p, val);
}

static void
localjump_error(mrb_state *mrb, localjump_error_kind kind)
{
  char kind_str[3][7] = { "return", "break", "yield" };
  char kind_str_len[] = { 6, 5, 5 };
  static const char lead[] = "unexpected ";
  mrb_value msg;
  mrb_value exc;

  msg = mrb_str_new_capa(mrb, sizeof(lead) + 7);
  mrb_str_cat(mrb, msg, lead, sizeof(lead) - 1);
  mrb_str_cat(mrb, msg, kind_str[kind], kind_str_len[kind]);
  exc = mrb_exc_new_str(mrb, E_LOCALJUMP_ERROR, msg);
  mrb_exc_set(mrb, exc);
}

void
mrbjit_localjump_error(mrb_state *mrb, localjump_error_kind kind)
{
  localjump_error(mrb, kind);
}

static void
argnum_error(mrb_state *mrb, mrb_int num)
{
  mrb_value exc;
  mrb_value str;
  mrb_int argc = mrb->c->ci->argc;

  if (argc < 0) {
    mrb_value args = mrb->c->stack[1];
    if (mrb_array_p(args)) {
      argc = RARRAY_LEN(args);
    }
  }
  if (mrb->c->ci->mid) {
    str = mrb_format(mrb, "'%S': wrong number of arguments (%S for %S)",
                  mrb_sym2str(mrb, mrb->c->ci->mid),
                  mrb_fixnum_value(argc), mrb_fixnum_value(num));
  }
  else {
    str = mrb_format(mrb, "wrong number of arguments (%S for %S)",
                     mrb_fixnum_value(argc), mrb_fixnum_value(num));
  }
  exc = mrb_exc_new_str(mrb, E_ARGUMENT_ERROR, str);
  mrb_exc_set(mrb, exc);
}

void
mrbjit_argnum_error(mrb_state *mrb, int num)
{
  argnum_error(mrb, num);
}

extern const void *mrbjit_get_curr(mrb_state *);
extern const void *mrbjit_emit_code(mrb_state *, mrbjit_vmstatus *, mrbjit_code_info *);
extern void mrbjit_gen_exit(mrbjit_code_area, mrb_state *, mrb_irep *, mrb_code **, mrbjit_vmstatus *, mrbjit_code_info *);
extern void mrbjit_gen_exit2(mrbjit_code_area, mrb_state *, mrb_irep *, mrb_code **, mrbjit_vmstatus *, mrbjit_code_info *);
extern const void *mrbjit_gen_jump_block(mrbjit_code_area, mrb_state *, void *, mrbjit_vmstatus *, mrbjit_code_info *, mrbjit_code_info *);
extern void mrbjit_gen_jmp_patch(mrb_state *mrb, mrbjit_code_area, void *, void *, mrbjit_vmstatus *, mrbjit_code_info *);
extern void mrbjit_gen_load_patch(mrbjit_code_area, mrb_state *, void *, void *, mrbjit_vmstatus *, mrbjit_code_info *);
extern void mrbjit_gen_align(mrbjit_code_area, unsigned);

static inline mrbjit_code_info *
mrbjit_search_codeinfo_prev_inline(mrbjit_codetab *tab, mrb_code *prev_pc, mrb_code *caller_pc, uint16_t arg_ver)
{
  volatile int i;		/* volatile avoid bug (maybe gcc?) */
  mrbjit_code_info *entry;

  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->prev_pc == prev_pc && 
	entry->caller_pc == caller_pc && 
	entry->method_arg_ver == arg_ver && entry->used) {
      return entry;
    }
  }

  return NULL;
}

mrbjit_code_info *
mrbjit_search_codeinfo_prev(mrbjit_codetab *tab, mrb_code *prev_pc, mrb_code *caller_pc, uint16_t arg_ver)
{
  return mrbjit_search_codeinfo_prev_inline(tab, prev_pc, caller_pc, arg_ver);
}

static inline mrbjit_code_info *
add_codeinfo(mrb_state *mrb, mrbjit_codetab *tab, mrb_irep *irep)
{
  int i;
  int oldsize;
  mrbjit_code_info *ele;
  oldsize = -1;

 retry:
  if (tab->body == NULL || oldsize >= 0) {
    oldsize = tab->size;
    tab->size = oldsize + (oldsize >> 1) + 2;
    tab->body = mrb_realloc(mrb, tab->body, sizeof(mrbjit_code_info) * tab->size);
    for (i = oldsize; i < tab->size; i++) {
      tab->body[i].used = 0;
      tab->body[i].reginfo = NULL;
      tab->body[i].patch_pos = NULL;
      tab->body[i].entry = NULL;
      tab->body[i].prev_pc = NULL;
      tab->body[i].caller_pc = NULL;
      tab->body[i].method_arg_ver = 0;
    }
  }

  oldsize = tab->size;
  for (i = 0; i < tab->size; i++) {
    ele = tab->body + i;
    if (ele->used == 0) {
      return ele;
    }
  }

  /* Grow code info table */
  goto retry;
}

mrbjit_code_info *
mrbjit_add_codeinfo(mrb_state *mrb, mrbjit_codetab *tab, mrb_irep *irep)
{
  return add_codeinfo(mrb, tab, irep);
}

static void
mrbjit_arth_overflow(mrb_state *mrb, mrb_irep *irep, mrbjit_code_area cbase, mrbjit_vmstatus *status)
{
  mrbjit_codetab *tab = irep->jit_entry_tab;
  mrbjit_code_info *entry;
  int i, j;

  /* irep->may_overflow == 0  always true */

  irep->may_overflow = 1;
  for (i = 0; i < tab->size; i++) {
    entry = tab->body + i;
    if (entry->used > 0) {
      mrbjit_gen_exit_patch(cbase, mrb, (void *)entry->entry,
			    irep->iseq, status, entry);
    }
  }
  for (j = 0; j < irep->ilen; j++) {
    for (i = 0; i < tab->size; i++) {
      entry = tab[j].body + i;
      if (entry->used > 0) {
	entry->used = -1;
	entry->entry = 0;
	if (entry->reginfo) {
	  mrb_free(mrb, entry->reginfo);
	}
	entry->reginfo = NULL;
      }
    }
  }
  irep->entry = NULL;
}

extern void disasm_once(mrb_state *, mrb_irep *, mrb_code);
extern void disasm_irep(mrb_state *, mrb_irep *);
extern void *mrbjit_invoke(mrb_value *, mrb_code **, mrb_state *, 
			   struct mrb_context *, void *, 
			   void *(**)());
#define GET_CODE_INFO(pc, toff) ({                        \
      mrbjit_codetab *ctab = irep->jit_entry_tab + ISEQ_OFFSET_OF((pc)); \
      ctab->body + toff;			                         \
})

int
mrb_check_export_reps(mrb_state *mrb, mrb_irep *irep, mrb_int drno, int level)
{
  int i;
  mrb_code *pc;
  mrb_code ins;
  for (i = 0; i < irep->rlen; i++) {
    if (mrb_check_export_reps(mrb, irep->reps[i], drno, level + 1)) {
      return 1;
    }
  }

  for (i = 0, pc = irep->iseq; i < irep->ilen; i++, pc++) {
    ins = *pc;
    //disasm_once(mrb, irep, ins);
    switch(GET_OPCODE(ins)) {
    case OP_GETUPVAR:
    case OP_SETUPVAR:
      if (GETARG_B(ins) == drno && GETARG_C(ins) == level) {
	return 1;
      }
      break;

    default:
      break;
    }
  }

  return 0;
}

int
mrb_patch_irep_var2fix(mrb_state *mrb, mrb_irep *irep, mrb_int drno)
{
  int i;
  mrb_code *oiseq = irep->iseq;
  mrb_code *pc;
  mrb_code ins;
  mrb_int tdrno = drno;

  for (i = 0; i < irep->rlen; i++) {
    if (mrb_check_export_reps(mrb, irep->reps[i], drno, 0)) {
      return 0;
    }
  }

  irep->iseq = (mrb_code*)mrb_malloc(mrb, sizeof(mrb_code)*irep->ilen);
  for (i = 0, pc = oiseq; i < irep->ilen; i++, pc++) {
    ins = *pc;
    irep->iseq[i] = ins;

    if (GET_OPCODE(ins) == OP_ARRAY &&
	GET_OPCODE(*(pc + 1)) == OP_ARYCAT &&
	GETARG_B(*(pc + 1)) == drno &&
	GETARG_C(ins) == 0) {

      if (GET_OPCODE(*(pc + 2)) == OP_SEND &&
	  GETARG_C(*(pc + 2)) == 127) {
	irep->iseq[i++] = MKOP_A(OP_NOP, 0);
	pc++;
	ins = *pc;
	irep->iseq[i++] = MKOP_AB(OP_MOVE, GETARG_A(ins), GETARG_B(ins));
	pc++;
	ins = *pc;
	irep->iseq[i] = MKOP_ABC(OP_SEND, GETARG_A(ins), GETARG_B(ins), 1);
      }
      else if (GET_OPCODE(*(pc + 2)) == OP_TAILCALL &&
	  GETARG_C(*(pc + 2)) == 127) {
	irep->iseq[i++] = MKOP_A(OP_NOP, 0);
	pc++;
	ins = *pc;
	irep->iseq[i++] = MKOP_AB(OP_MOVE, GETARG_A(ins), GETARG_B(ins));
	pc++;
	ins = *pc;
	irep->iseq[i] = MKOP_ABC(OP_TAILCALL, GETARG_A(ins), GETARG_B(ins), 1);
      }
      else if (GET_OPCODE(*(pc + 3)) == OP_SENDB &&
	       GETARG_C(*(pc + 3)) == 127) {
	irep->iseq[i++] = MKOP_A(OP_NOP, 0);
	pc++;
	ins = *pc;
	irep->iseq[i++] = MKOP_AB(OP_MOVE, GETARG_A(ins), GETARG_B(ins));
	pc++;
	irep->iseq[i++] = *(pc++);
	ins = *pc;
	irep->iseq[i] = MKOP_ABC(OP_SENDB, GETARG_A(ins), GETARG_B(ins), 1);
      }
      else {
	mrb_free(mrb, irep->iseq);
	irep->iseq = oiseq;
	return 0;
      }
    }

    if (GET_OPCODE(ins) == OP_MOVE) {
      if (GETARG_B(ins) == drno) {
	tdrno = GETARG_A(ins);
      }
      else if (GETARG_A(ins) == drno ||
	       GETARG_B(ins) == tdrno) {
	mrb_free(mrb, irep->iseq);
	irep->iseq = oiseq;
	return 0;
      }
    }

    switch (GET_OPCODE(ins)) {
    case OP_RETURN:
    case OP_SETIV:
    case OP_SETCV:
    case OP_SETGLOBAL:
      if (   GETARG_A(ins) == tdrno
	  || GETARG_A(ins) == drno) {
	mrb_free(mrb, irep->iseq);
	irep->iseq = oiseq;
	return 0;
      }
      break;

    default:
      break;
    }

    if (GET_OPCODE(ins) == OP_SEND &&
	(GETARG_A(ins) == tdrno ||
	 (GETARG_C(ins) == 1 && GETARG_A(ins) + 1 == tdrno))) {
      if (GETARG_C(ins) == 127) {
	irep->iseq[i] = MKOP_ABC(OP_SEND, GETARG_A(ins), GETARG_B(ins), 1);
      }
      else if (strcmp(mrb_sym2name(mrb, irep->syms[GETARG_B(ins)]), "__svalue") == 0 ||
	       strcmp(mrb_sym2name(mrb, irep->syms[GETARG_B(ins)]), "[]") == 0) {
	irep->iseq[i] = MKOP_A(OP_NOP, 0);
      }
      else if (strcmp(mrb_sym2name(mrb, irep->syms[GETARG_B(ins)]), "size") == 0) {
	irep->iseq[i] = MKOP_AsBx(OP_LOADI, tdrno, 1);
      }
      else {
	mrb_free(mrb, irep->iseq);
	irep->iseq = oiseq;
	return 0;
      }

      tdrno = drno;
    }
    else if (GET_OPCODE(ins) == OP_TAILCALL &&
	(GETARG_A(ins) == tdrno ||
	 (GETARG_C(ins) == 1 && GETARG_A(ins) + 1 == tdrno))) {
      if (GETARG_C(ins) == 127) {
	irep->iseq[i] = MKOP_ABC(OP_TAILCALL, GETARG_A(ins), GETARG_B(ins), 1);
      }
      else if (strcmp(mrb_sym2name(mrb, irep->syms[GETARG_B(ins)]), "__svalue") == 0 ||
	       strcmp(mrb_sym2name(mrb, irep->syms[GETARG_B(ins)]), "[]") == 0) {
	irep->iseq[i] = MKOP_AB(OP_RETURN, GETARG_A(ins), OP_R_NORMAL);
      }
      else {
	mrb_free(mrb, irep->iseq);
	irep->iseq = oiseq;
	return 0;
      }
    }
    else {
      switch (GET_OPCODE(ins)) {
      case OP_SEND:
      case OP_SENDB:
      case OP_TAILCALL:
      case OP_ARRAY:
      case OP_HASH:
      case OP_ADD:
      case OP_SUB:
	if (GETARG_A(ins) <= tdrno &&
	    tdrno <= GETARG_A(ins) + GETARG_C(ins)) {
	  mrb_free(mrb, irep->iseq);
	  irep->iseq = oiseq;
	  return 0;
	}
	break;

      default:
	break;
      }
    }
  }

  return 1;
}

static inline void *
mrbjit_dispatch(mrb_state *mrb, mrbjit_vmstatus *status)
{
  mrb_irep *irep = *status->irep;
  mrb_code **ppc = status->pc;
  mrb_value *regs = mrb->c->stack;
  size_t n;
  mrbjit_code_info *ci;
  mrbjit_code_area cbase;
  mrb_code *prev_pc;
  uint16_t method_arg_ver;
  mrb_code *caller_pc;
  void *(*entry)() = NULL;
  void *(*prev_entry)() = NULL;
  void *rc = NULL;
  int i;

  if (mrb->compile_info.disable_jit ||
      irep->disable_jit ||
      irep->method_kind != NORMAL) {
    return status->optable[GET_OPCODE(**ppc)];
  }

  /* Check first instruction of block. 
     So clear compille infomation of previous VM instruction */
  if (irep->iseq == *ppc /*&& mrb->compile_info.force_compile == 0*/) {
    mrb->c->ci->prev_tentry_offset = -1;
    mrb->c->ci->prev_pc = NULL;
  }

  if (irep->jit_entry_tab == NULL) {
    mrbjit_make_jit_entry_tab(mrb, irep, irep->ilen);
  }

  prev_pc = mrb->c->ci->prev_pc;
  method_arg_ver = mrb->c->ci->method_arg_ver;
  if (mrb->compile_info.force_compile) {
    irep->arg_ver_num++;
    method_arg_ver = irep->arg_ver_num;
  }

  if (irep->jit_inlinep) {
    caller_pc = mrb->c->ci->pc;
  }
  else {
    caller_pc = NULL;
    mrb->compile_info.nest_level = 0;
  }

  cbase = mrb->compile_info.code_base;
  n = ISEQ_OFFSET_OF(*ppc);
  ci = mrbjit_search_codeinfo_prev_inline(irep->jit_entry_tab + n, prev_pc, caller_pc, method_arg_ver);
  if (ci) {
    entry = ci->entry;
    //printf("%x %x\n", ci, entry);
    if (cbase) {
      if (ci->used > 0) {
	mrbjit_code_info *prev_ci;

	if (prev_pc && mrb->c->ci->prev_tentry_offset != -1) {
	  prev_ci = GET_CODE_INFO(prev_pc, mrb->c->ci->prev_tentry_offset);
	}
	else {
	  prev_ci = NULL;
	}
	entry = mrbjit_gen_jump_block(cbase, mrb, entry, status, ci, prev_ci);
	cbase = mrb->compile_info.code_base = NULL;
      }
    }
    if (ci->used > 0) {
      prev_pc = *ppc;
      /* For compiled block through call operation */
      mrb->compile_info.force_compile = 0;

      //printf("%x %x \n", ci->entry, *ppc);
#if 0
      asm volatile("mov %0, %%ecx\n\t"
		   "mov %1, %%ebx\n\t"
		   "mov %2, %%esi\n\t"
		   "mov %3, %%edi\n\t"
		   :
		   : "g"(regs),
		     "g"(status->pc),
		     "g"(mrb),
		     "g"(mrb->c)
		   : "%ecx",
		     "%ebx",
		     "%esi",
		     "%edi",
		     "memory");

      asm volatile("call *%0\n\t"
		   :
		   : "g"(entry)
		   : );

      asm volatile("mov %%eax, %0\n\t"
		   : "=c"(rc));
      asm volatile("mov %%edx, %0\n\t"
		   : "=c"(prev_entry));
#else
      assert((*status->proc)->body.irep->ilen == (*status->irep)->ilen);
      rc = mrbjit_invoke(regs, status->pc, mrb, mrb->c, entry, &prev_entry);

#endif
      irep = *status->irep;
      regs = mrb->c->stack;
      *(status->pool) = irep->pool;
      *(status->syms) = irep->syms;
      n = ISEQ_OFFSET_OF(*ppc);
      assert(*status->pc >= irep->iseq && *status->pc <= irep->iseq + irep->ilen);

      if (prev_entry == (void *(*)())1) {
	/* Overflow happened */
	//puts("overflow");
	prev_entry = NULL;
	ci = NULL;
	mrbjit_arth_overflow(mrb, irep, cbase, status);
      }

      if (rc == (void *(*)())1) {
	/* Maybe arg guard fail */
	irep->arg_ver_num++;
	method_arg_ver = irep->arg_ver_num;
	//printf("new version %d \n", method_arg_ver);
	mrb->c->ci->prev_tentry_offset = -1;
	rc = NULL;
      }
      else if (rc == (void *(*)())3) {
	/* Guard JMPIF/JMPNOT fail */
	method_arg_ver = mrb->c->ci->method_arg_ver;
	mrb->c->ci->prev_tentry_offset = -1;
	rc = NULL;
      }
      else if (rc == (void *(*)())5) {
	/* block guard fail need stack overflow check */
	//method_arg_ver = mrb->c->ci->method_arg_ver;
	irep->arg_ver_num++;
	method_arg_ver = irep->arg_ver_num;
	//mrb->c->ci->prev_tentry_offset = -1;
	
	if (mrb->c->ci->argc == -1) {
	  mrbjit_stack_extend(mrb, (irep->nregs < 3) ? 3 : irep->nregs);
	}
	else {
	  mrbjit_stack_extend(mrb, irep->nregs);
	}
	rc = NULL;
      }
      else if (GET_OPCODE(*(*ppc - 1)) == OP_SEND ||
	       GET_OPCODE(*(*ppc - 1)) == OP_SENDB) {
	method_arg_ver = mrb->c->ci->method_arg_ver;
	mrb->c->ci->prev_tentry_offset = -1;
      }
      else {
	method_arg_ver = mrb->c->ci->method_arg_ver;
	mrb->c->ci->prev_tentry_offset = -1;
      }

      prev_pc = mrb->c->ci->prev_pc;
      //disasm_once(mrb, irep, **ppc);
      //mrb_irep *search_irep(mrb_state *mrb, mrb_code *pc);
      //if (search_irep(mrb, *ppc) != irep) {
      //printf("%x %x %x \n", irep, *ppc, prev_entry);
      //puts("foo");
      //}

      if (rc) {
	ci = NULL;
	goto skip;
      }

      if (irep->jit_inlinep) {
	caller_pc = mrb->c->ci->pc;
      }
      else {
	caller_pc = NULL;
	mrb->compile_info.nest_level = 0;
      }
      if (irep->jit_entry_tab == NULL) {
	mrbjit_make_jit_entry_tab(mrb, irep, irep->ilen);
      }

      if (irep->iseq == *ppc && mrb->compile_info.force_compile == 0) {
	prev_pc = mrb->c->ci->prev_pc = NULL;
      }

      /*switch (GET_OPCODE(*(*ppc - 1))) {
      case OP_SEND:
      case OP_SENDB:
	mrb->c->ci->prev_pc = prev_pc = *ppc - 1;
	if (mrb->c->ci->prev_tentry_offset == -1) {
	  mrbjit_codetab *ctab = irep->jit_entry_tab + ISEQ_OFFSET_OF(*ppc - 1);
	  int i;

	  for (i = ctab->size - 1; i <= 0; i--) {
	    if (ctab->body[i].used) {
	      break;
	    }
	  }
	  mrb->c->ci->prev_tentry_offset = i;
	}
	break;
	}*/

      ci = mrbjit_search_codeinfo_prev_inline(irep->jit_entry_tab + n, prev_pc, caller_pc, method_arg_ver);
    }
  }

  if (irep->prof_info[n]++ > COMPILE_THRESHOLD ||
      mrb->compile_info.force_compile) {
    struct mrbjit_code_info *prev_coi;
    //      printf("size %x %x %x\n", irep->jit_entry_tab[n].size, *ppc, prev_pc);
    if (ci == NULL) {
      //printf("p %x %x\n", *ppc, prev_pc);
      ci = add_codeinfo(mrb, irep->jit_entry_tab + n, irep);
      ci->prev_pc = prev_pc;
      ci->method_arg_ver = method_arg_ver;
      ci->caller_pc = caller_pc;
      ci->entry = NULL;
      ci->used = -1;
    }

    if (ci->reginfo == NULL) {
      ci->reginfo = (mrbjit_reginfo *)mrb_calloc(mrb, irep->nregs + 1, sizeof(mrbjit_reginfo));
      for (i = 0; i < irep->nregs + 1; i++) {
	ci->reginfo[i].type = MRB_TT_FREE;
	ci->reginfo[i].klass = NULL;
	ci->reginfo[i].constp = 0;
	ci->reginfo[i].unboxedp = 0;
	ci->reginfo[i].regplace = MRBJIT_REG_MEMORY;
      }
    }
    if (prev_pc && mrb->c->ci->prev_tentry_offset != -1) {
      prev_coi = GET_CODE_INFO(prev_pc, mrb->c->ci->prev_tentry_offset);
    }
    else {
      prev_coi = NULL;
    }

    if (prev_coi && prev_coi->reginfo) {
      mrbjit_reginfo *prev_rinfo = prev_coi->reginfo;

      for (i = 0; i < irep->nregs + 1; i++) {
	ci->reginfo[i] = prev_rinfo[i];
      }
    }

    if (ci->used > 0) {
      goto skip;
    }
    if (GET_OPCODE(*irep->iseq) != OP_CALL) {
      int ioff;
      int toff;
      mrbjit_codetab *ctab;

      ioff = ISEQ_OFFSET_OF(*ppc);
      toff = ci - (irep->jit_entry_tab + ioff)->body;

      entry = mrbjit_emit_code(mrb, status, ci);

      /* Update ci for realloc in ent_send(gen_set_jit_entry) */
      ctab = (irep->jit_entry_tab + ioff);
      ci = ctab->body + toff;

      if (prev_entry && entry) {
	//printf("patch %x %x \n", prev_entry, entry);
	cbase = mrb->compile_info.code_base;
	mrbjit_gen_jmp_patch(mrb, cbase, prev_entry, entry, status, ci);
	prev_entry = NULL;
      }

      if (entry && mrb->compile_info.ignor_inst_cnt == 0) {
	ci->entry = entry;
	ci->used = 1;
      }
      else {
	/* record contination patch entry */
	ci->used = -1;
	if (cbase) {
	  ci->entry = mrbjit_get_curr(cbase);
	}
	//	printf("set %x %x \n", ci->entry, entry);
	// printf("%x %x %x\n", ci->entry, *ppc, ci);
      }
    }
  }
   
  if (cbase && irep->prof_info[n] > 0 &&
      entry == NULL &&
      GET_OPCODE(*irep->iseq) != OP_CALL &&
      !mrb->compile_info.force_compile) {
    /* Finish compile */
    mrbjit_gen_exit(cbase, mrb, irep, ppc, status, ci);
    //mrbjit_gen_align(cbase, 16);
    mrb->compile_info.code_base = NULL;
    mrb->compile_info.nest_level = 0;
    ci = NULL;
  }

 skip:

  /* This cause infinit loop in make test */
  if (prev_entry && ci && ci->used > 0 && prev_entry != ci->entry) {
    cbase = mrb->compile_info.code_base;
    mrbjit_gen_jmp_patch(mrb, cbase, prev_entry, ci->entry, status, ci);
  }

  if (mrb->compile_info.ignor_inst_cnt > 0) {
    mrb->compile_info.ignor_inst_cnt--;
  }
  mrb->c->ci->prev_pc = *ppc;
  mrb->c->ci->method_arg_ver = method_arg_ver;
  if (ci) {
    if (ci->patch_pos && ci->used > 0) {
      mrbjit_gen_load_patch(cbase, mrb, (void *)ci->patch_pos, ci->entry, status, ci);
      ci->patch_pos = NULL;
    }
    mrb->c->ci->prev_tentry_offset = ci - (irep->jit_entry_tab + ISEQ_OFFSET_OF(*ppc))->body;
    assert(mrb->c->ci->prev_tentry_offset >= 0);
  }
  else {
    mrb->c->ci->prev_tentry_offset = -1;
  }

  if (rc) {
    return rc;
  }
  return status->optable[GET_OPCODE(**ppc)];
}

#define ERR_PC_SET(mrb, pc) mrb->c->ci->err = pc;
#define ERR_PC_CLR(mrb)     mrb->c->ci->err = 0;
#ifdef MRB_ENABLE_DEBUG_HOOK
#define CODE_FETCH_HOOK(mrb, irep, pc, regs) if ((mrb)->code_fetch_hook) (mrb)->code_fetch_hook((mrb), (irep), (pc), (regs));
#else
#define CODE_FETCH_HOOK(mrb, irep, pc, regs)
#endif

#ifdef MRB_BYTECODE_DECODE_OPTION
#define BYTECODE_DECODER(x) ((mrb)->bytecode_decoder)?(mrb)->bytecode_decoder((mrb), (x)):(x)
#else
#define BYTECODE_DECODER(x) (x)
#endif


#if defined __GNUC__ || defined __clang__ || defined __INTEL_COMPILER
#define DIRECT_THREADED
#endif

#ifndef DIRECT_THREADED

/* You can not execute by JIT sorry... */
#define INIT_DISPATCH for (;;) { i = BYTECODE_DECODER(*pc); CODE_FETCH_HOOK(mrb, irep, pc, regs);switch (GET_OPCODE(i)) {
#define CASE(op) case op:
#define NEXT pc++; break
#define JUMP break
#define END_DISPATCH }}

#else

#define INIT_DISPATCH JUMP; return mrb_nil_value();
#define CASE(op) L_ ## op:
#define NEXT ++pc;goto L_DISPATCH
#define JUMP goto L_DISPATCH

#define END_DISPATCH

#endif

MRB_API mrb_value
mrb_vm_run(mrb_state *mrb, struct RProc *proc, mrb_value self, unsigned int stack_keep)
{
  mrb_irep *irep = proc->body.irep;
  mrb_value result;
  struct mrb_context *c = mrb->c;
  ptrdiff_t cioff = c->ci - c->cibase;
  unsigned int nregs = irep->nregs;

  if (!c->stack) {
    stack_init(mrb);
  }
  if (stack_keep > nregs)
    nregs = stack_keep;
  stack_extend(mrb, nregs);
  stack_clear(mrb, c->stack + stack_keep, nregs - stack_keep);
  c->stack[0] = self;
  result = mrb_vm_exec(mrb, proc, irep->iseq);
  if (c->ci - c->cibase > cioff) {
    c->ci = c->cibase + cioff;
  }
  if (mrb->c != c) {
    if (mrb->c->fib) {
      mrb_write_barrier(mrb, (struct RBasic*)mrb->c->fib);
    }
    mrb->c = c;
  }
  return result;
}

MRB_API mrb_value
mrb_vm_exec(mrb_state *mrb, struct RProc *proc, mrb_code *pc)
{
  /* mrb_assert(mrb_proc_cfunc_p(proc)) */
  mrb_irep *irep = proc->body.irep;
  mrb_value *pool = irep->pool;
  mrb_sym *syms = irep->syms;
  mrb_code i;
  int ai = mrb_gc_arena_save(mrb);
  struct mrb_jmpbuf *prev_jmp = mrb->jmp;
  struct mrb_jmpbuf c_jmp;
  int orgdisflg = mrb->compile_info.disable_jit;

#ifdef DIRECT_THREADED
  static void *optable[] = {
    &&L_OP_NOP, &&L_OP_MOVE,
    &&L_OP_LOADL, &&L_OP_LOADI, &&L_OP_LOADSYM, &&L_OP_LOADNIL,
    &&L_OP_LOADSELF, &&L_OP_LOADT, &&L_OP_LOADF,
    &&L_OP_GETGLOBAL, &&L_OP_SETGLOBAL, &&L_OP_GETSPECIAL, &&L_OP_SETSPECIAL,
    &&L_OP_GETIV, &&L_OP_GETIV2, &&L_OP_SETIV, &&L_OP_SETIV2, &&L_OP_GETCV, &&L_OP_SETCV,
    &&L_OP_GETCONST, &&L_OP_SETCONST, &&L_OP_GETMCNST, &&L_OP_SETMCNST,
    &&L_OP_GETUPVAR, &&L_OP_SETUPVAR,
    &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_JMPNOT,
    &&L_OP_ONERR, &&L_OP_RESCUE, &&L_OP_POPERR, &&L_OP_RAISE, &&L_OP_EPUSH, &&L_OP_EPOP,
    &&L_OP_SEND, &&L_OP_SENDB, &&L_OP_FSEND,
    &&L_OP_CALL, &&L_OP_SUPER, &&L_OP_ARGARY, &&L_OP_ENTER,
    &&L_OP_KARG, &&L_OP_KDICT, &&L_OP_RETURN, &&L_OP_TAILCALL, &&L_OP_BLKPUSH,
    &&L_OP_ADD, &&L_OP_ADDI, &&L_OP_SUB, &&L_OP_SUBI, &&L_OP_MUL, &&L_OP_DIV,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_GT, &&L_OP_GE,
    &&L_OP_ARRAY, &&L_OP_ARYCAT, &&L_OP_ARYPUSH, &&L_OP_AREF, &&L_OP_ASET, &&L_OP_APOST,
    &&L_OP_STRING, &&L_OP_STRCAT, &&L_OP_HASH,
    &&L_OP_LAMBDA, &&L_OP_RANGE, &&L_OP_OCLASS,
    &&L_OP_CLASS, &&L_OP_MODULE, &&L_OP_EXEC,
    &&L_OP_METHOD, &&L_OP_SCLASS, &&L_OP_TCLASS,
    &&L_OP_DEBUG, &&L_OP_STOP, &&L_OP_ERR,
  };

  void *gtptr;			/* Use in NEXT/JUMP */
  mrb_bool exc_catched = FALSE;

  void *gototable[] = {
    &&L_RAISE, &&L_RETURN, &&L_RESCUE, &&L_SEND, &&L_STOP, &&L_HALT, &&L_DISPATCH,
  };
#endif

  mrbjit_vmstatus status = {
    &irep, &proc, &pc, &pool, &syms, &ai, &status,
    optable, gototable, &prev_jmp
  };

  mrb->vmstatus = (void *)&status;
  irep = proc->body.irep;
  pool = irep->pool;
  syms = irep->syms;
  ai = mrb_gc_arena_save(mrb);
  prev_jmp = mrb->jmp;

  mrb->compile_info.nest_level = 0;
  mrb->c->ci->prev_pc = NULL;
  mrb->c->ci->method_arg_ver = 0;
  mrb->c->ci->prev_tentry_offset = -1;

RETRY_TRY_BLOCK:

  MRB_TRY(&c_jmp) {

  if (exc_catched) {
    exc_catched = FALSE;
    if (mrb->exc && mrb->exc->tt == MRB_TT_BREAK)
      goto L_BREAK;
    goto L_RAISE;
  }
  mrb->jmp = &c_jmp;
  mrb->c->ci->proc = proc;
  mrb->c->ci->nregs = irep->nregs;

#define regs (mrb->c->stack)
  INIT_DISPATCH {
    CASE(OP_NOP) {
      /* do nothing */
      NEXT;
    }

    CASE(OP_MOVE) {
      /* A B    R(A) := R(B) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      regs[a] = regs[b];
      NEXT;
    }

    CASE(OP_LOADL) {
      /* A Bx   R(A) := Pool(Bx) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
#ifdef MRB_WORD_BOXING
      mrb_value val = pool[bx];
#ifndef MRB_WITHOUT_FLOAT
      if (mrb_float_p(val)) {
        val = mrb_float_value(mrb, mrb_float(val));
      }
#endif
      regs[a] = val;
#else
      regs[a] = pool[bx];
#endif
      NEXT;
    }

    CASE(OP_LOADI) {
      /* A sBx  R(A) := sBx */
      int a = GETARG_A(i);
      mrb_int bx = GETARG_sBx(i);
      SET_INT_VALUE(regs[a], bx);
      NEXT;
    }

    CASE(OP_LOADSYM) {
      /* A Bx   R(A) := Syms(Bx) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      SET_SYM_VALUE(regs[a], syms[bx]);
      NEXT;
    }

    CASE(OP_LOADSELF) {
      /* A      R(A) := self */
      int a = GETARG_A(i);
      regs[a] = regs[0];
      NEXT;
    }

    CASE(OP_LOADT) {
      /* A      R(A) := true */
      int a = GETARG_A(i);
      SET_TRUE_VALUE(regs[a]);
      NEXT;
    }

    CASE(OP_LOADF) {
      /* A      R(A) := false */
      int a = GETARG_A(i);
      SET_FALSE_VALUE(regs[a]);
      NEXT;
    }

    CASE(OP_GETGLOBAL) {
      /* A Bx   R(A) := getglobal(Syms(Bx)) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_value val = mrb_gv_get(mrb, syms[bx]);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETGLOBAL) {
      /* A Bx   setglobal(Syms(Bx), R(A)) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_gv_set(mrb, syms[bx], regs[a]);
      NEXT;
    }

    CASE(OP_GETSPECIAL) {
      /* A Bx   R(A) := Special[Bx] */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_value val = mrb_vm_special_get(mrb, bx);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETSPECIAL) {
      /* A Bx   Special[Bx] := R(A) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_vm_special_set(mrb, bx, regs[a]);
      NEXT;
    }

    CASE(OP_GETIV) {
      /* A Bx   R(A) := ivget(Bx) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_value val = mrb_vm_iv_get(mrb, syms[bx]);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_GETIV2) {
      /* A Bx   R(A) := ivget(Bx) of R(A) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_value val = mrb_iv_get(mrb, regs[a], syms[bx]);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETIV) {
      /* A Bx   ivset(Syms(Bx),R(A)) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_vm_iv_set(mrb, syms[bx], regs[a]);
      NEXT;
    }

    CASE(OP_SETIV2) {
      /* A Bx   ivset(Syms(Bx),R(A)) of R(A) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_iv_set(mrb, regs[a], syms[bx], regs[a]);
      NEXT;
    }

    CASE(OP_GETCV) {
      /* A Bx   R(A) := cvget(Syms(Bx)) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_value val;
      ERR_PC_SET(mrb, pc);
      val = mrb_vm_cv_get(mrb, syms[bx]);
      ERR_PC_CLR(mrb);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETCV) {
      /* A Bx   cvset(Syms(Bx),R(A)) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_vm_cv_set(mrb, syms[bx], regs[a]);
      NEXT;
    }

    CASE(OP_GETCONST) {
      /* A Bx    R(A) := constget(Syms(Bx)) */
      mrb_value val;
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_sym sym = syms[bx];

      ERR_PC_SET(mrb, pc);
      val = mrb_vm_const_get(mrb, sym);
      ERR_PC_CLR(mrb);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETCONST) {
      /* A Bx   constset(Syms(Bx),R(A)) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_vm_const_set(mrb, syms[bx], regs[a]);
      NEXT;
    }

    CASE(OP_GETMCNST) {
      /* A Bx   R(A) := R(A)::Syms(Bx) */
      mrb_value val;
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);

      ERR_PC_SET(mrb, pc);
      val = mrb_const_get(mrb, regs[a], syms[bx]);
      ERR_PC_CLR(mrb);
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETMCNST) {
      /* A Bx    R(A+1)::Syms(Bx) := R(A) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_const_set(mrb, regs[a+1], syms[bx], regs[a]);
      NEXT;
    }

    CASE(OP_GETUPVAR) {
      /* A B C  R(A) := uvget(B,C) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      mrb_value *regs_a = regs + a;
      struct REnv *e = uvenv(mrb, c);

      if (!e) {
        *regs_a = mrb_nil_value();
      }
      else {
        *regs_a = e->stack[b];
      }
      NEXT;
    }

    CASE(OP_SETUPVAR) {
      /* A B C  uvset(B,C,R(A)) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int c = GETARG_C(i);

      struct REnv *e = uvenv(mrb, c);

      if (e) {
        mrb_value *regs_a = regs + a;

        if (b < MRB_ENV_STACK_LEN(e)) {
          e->stack[b] = *regs_a;
          mrb_write_barrier(mrb, (struct RBasic*)e);
        }
      }
      NEXT;
    }

    CASE(OP_JMP) {
      /* sBx    pc+=sBx */
      int sbx = GETARG_sBx(i);
      pc += sbx;
      JUMP;
    }

    CASE(OP_JMPIF) {
      /* A sBx  if R(A) pc+=sBx */
      int a = GETARG_A(i);
      int sbx = GETARG_sBx(i);
      if (mrb_test(regs[a])) {
        pc += sbx;
        JUMP;
      }
      NEXT;
    }

    CASE(OP_JMPNOT) {
      /* A sBx  if !R(A) pc+=sBx */
      int a = GETARG_A(i);
      int sbx = GETARG_sBx(i);
      if (!mrb_test(regs[a])) {
        pc += sbx;
        JUMP;
      }
      NEXT;
    }

    CASE(OP_ONERR) {
      /* sBx    pc+=sBx on exception */
      int sbx = GETARG_sBx(i);
      if (mrb->c->rsize <= mrb->c->ci->ridx) {
        if (mrb->c->rsize == 0) mrb->c->rsize = RESCUE_STACK_INIT_SIZE;
        else mrb->c->rsize *= 2;
        mrb->c->rescue = (mrb_code **)mrb_realloc(mrb, mrb->c->rescue, sizeof(mrb_code*) * mrb->c->rsize);
      }
      mrb->c->rescue[mrb->c->ci->ridx++] = pc + sbx;
      NEXT;
    }

    CASE(OP_RESCUE) {
      /* A B    R(A) := exc; clear(exc); R(B) := matched (bool) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      mrb_value exc;

      if (c == 0) {
        exc = mrb_obj_value(mrb->exc);
        mrb->exc = 0;
      }
      else {           /* continued; exc taken from R(A) */
        exc = regs[a];
      }
      if (b != 0) {
        mrb_value e = regs[b];
        struct RClass *ec;

        switch (mrb_type(e)) {
        case MRB_TT_CLASS:
        case MRB_TT_MODULE:
          break;
        default:
          {
            mrb_value exc;

            exc = mrb_exc_new_str_lit(mrb, E_TYPE_ERROR,
                  "class or module required for rescue clause");
            mrb_exc_set(mrb, exc);
            goto L_RAISE;
          }
        }
        ec = mrb_class_ptr(e);
        regs[b] = mrb_bool_value(mrb_obj_is_kind_of(mrb, exc, ec));
      }
      if (a != 0 && c == 0) {
        regs[a] = exc;
      }
      NEXT;
    }

    CASE(OP_POPERR) {
      /* A      A.times{rescue_pop()} */
      int a = GETARG_A(i);

      mrb->c->ci->ridx -= a;
      NEXT;
    }

    CASE(OP_RAISE) {
      /* A      raise(R(A)) */
      int a = GETARG_A(i);

      mrb_exc_set(mrb, regs[a]);
      goto L_RAISE;
    }

    CASE(OP_EPUSH) {
      /* Bx     ensure_push(SEQ[Bx]) */
      int bx = GETARG_Bx(i);
      struct RProc *p;

      p = mrb_closure_new(mrb, irep->reps[bx]);
      /* push ensure_stack */
      if (mrb->c->esize <= mrb->c->eidx+1) {
        if (mrb->c->esize == 0) mrb->c->esize = ENSURE_STACK_INIT_SIZE;
        else mrb->c->esize *= 2;
        mrb->c->ensure = (struct RProc **)mrb_realloc(mrb, mrb->c->ensure, sizeof(struct RProc*) * mrb->c->esize);
      }
      mrb->c->ensure[mrb->c->eidx++] = p;
      mrb->c->ensure[mrb->c->eidx] = NULL;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_EPOP) {
      /* A      A.times{ensure_pop().call} */
      int a = GETARG_A(i);
      mrb_callinfo *ci = mrb->c->ci;
      int n, epos = ci->epos;
      mrb_value self = regs[0];
      struct RClass *target_class = ci->target_class;

      if (mrb->c->eidx <= epos) {
        NEXT;
      }

      if (a > mrb->c->eidx - epos)
        a = mrb->c->eidx - epos;
      pc = pc + 1;
      for (n=0; n<a; n++) {
        proc = mrb->c->ensure[epos+n];
        mrb->c->ensure[epos+n] = NULL;
        irep = proc->body.irep;
        ci = cipush(mrb);
        ci->mid = ci[-1].mid;
        ci->argc = 0;
        ci->proc = proc;
        ci->stackent = mrb->c->stack;
        ci->nregs = irep->nregs;
        ci->target_class = target_class;
        ci->pc = pc;
        ci->acc = ci[-1].nregs;
        mrb->c->stack += ci->acc;
        stack_extend(mrb, ci->nregs);
        regs[0] = self;
        pc = irep->iseq;
      }
      pool = irep->pool;
      syms = irep->syms;
      mrb->c->eidx = epos;
      JUMP;
    }

    CASE(OP_LOADNIL) {
      /* A     R(A) := nil */
      int a = GETARG_A(i);

      SET_NIL_VALUE(regs[a]); 

      NEXT;
    }

    CASE(OP_SENDB) {
      /* A B C  R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C),&R(A+C+1))*/
      /* fall through */
    };

  L_SEND:
    CASE(OP_SEND) {
      /* A B C  R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C)) */
      int a = GETARG_A(i);
      int n = GETARG_C(i);
      int argc = (n == CALL_MAXARGS) ? -1 : n;
      int bidx = (argc < 0) ? a+2 : a+n+1;
      mrb_method_t m;
      struct RClass *c;
      mrb_callinfo *ci = mrb->c->ci;
      mrb_value recv, blk;
      mrb_sym mid = syms[GETARG_B(i)];

      mrb_assert(bidx < ci->nregs);

      recv = regs[a];
      if (GET_OPCODE(i) != OP_SENDB) {
        SET_NIL_VALUE(regs[bidx]);
        blk = regs[bidx];
      }
      else {
        blk = regs[bidx];
        if (!mrb_nil_p(blk) && mrb_type(blk) != MRB_TT_PROC) {
          blk = mrb_convert_type(mrb, blk, MRB_TT_PROC, "Proc", "to_proc");
          /* The stack might have been reallocated during mrb_convert_type(),
             see #3622 */
          regs[bidx] = blk;
        }
      }
      c = mrb_class(mrb, recv);

      m = mrb_method_search_vm(mrb, &c, mid);
      if (MRB_METHOD_UNDEF_P(m)) {
        mrb_sym missing = mrb_intern_lit(mrb, "method_missing");
        m = mrb_method_search_vm(mrb, &c, missing);
        if (MRB_METHOD_UNDEF_P(m) || (missing == mrb->c->ci->mid && mrb_obj_eq(mrb, regs[0], recv))) {
          mrb_value args = (argc < 0) ? regs[a+1] : mrb_ary_new_from_values(mrb, n, regs+a+1);
          ERR_PC_SET(mrb, pc);
          mrb_method_missing(mrb, mid, recv, args);
        }
        if (argc >= 0) {
          if (a+2 >= irep->nregs) {
            stack_extend(mrb, a+3);
          }
          regs[a+1] = mrb_ary_new_from_values(mrb, n, regs+a+1);
          regs[a+2] = blk;
          argc = -1;
        }
        mrb_ary_unshift(mrb, regs[a+1], mrb_symbol_value(mid));
        mid = missing;
      }

      if (MRB_DMETHOD_CFUNC_P(m)) {
        regs[a] = (MRB_DMETHOD_CFUNC(m))(mrb, regs + a, argc);
        mrb_gc_arena_restore(mrb, ai);
        mrb_gc_arena_shrink(mrb, ai);
        if (mrb->exc) goto L_RAISE;

	NEXT;
      }
      /* push callinfo */
      ci = cipush(mrb);
      ci->mid = mid;
      ci->stackent = mrb->c->stack;
      ci->target_class = c;
      ci->argc = argc;

      ci->pc = pc + 1;
      ci->acc = a;

      /* prepare stack */
      mrb->c->stack += a;

      if (MRB_METHOD_CFUNC_P(m)) {
        ci->nregs = (argc < 0) ? 3 : n+2;
	mrb->compile_info.disable_jit = 1;
        if (MRB_METHOD_PROC_P(m)) {
          struct RProc *p = MRB_METHOD_PROC(m);

          ci->proc = p;
          recv = p->body.func(mrb, recv);
        }
        else {
          recv = MRB_METHOD_FUNC(m)(mrb, recv);
        }
	mrb->compile_info.disable_jit = orgdisflg;

        mrb_gc_arena_restore(mrb, ai);
        mrb_gc_arena_shrink(mrb, ai);
        if (mrb->exc) goto L_RAISE;
        ci = mrb->c->ci;
        if (GET_OPCODE(i) == OP_SENDB) {
          if (mrb_type(blk) == MRB_TT_PROC) {
            struct RProc *p = mrb_proc_ptr(blk);
            if (p && !MRB_PROC_STRICT_P(p) && MRB_PROC_ENV(p) == ci[-1].env) {
              p->flags |= MRB_PROC_ORPHAN;
            }
          }
        }
        if (!ci->target_class) { /* return from context modifying method (resume/yield) */
          if (ci->acc == CI_ACC_RESUMED) {
            mrb->jmp = prev_jmp;
            return recv;
          }
          else {
            mrb_assert(!MRB_PROC_CFUNC_P(ci[-1].proc));
            proc = ci[-1].proc;
            irep = proc->body.irep;
            pool = irep->pool;
            syms = irep->syms;
          }
        }
        mrb->c->stack[0] = recv;
        /* pop stackpos */
        mrb->c->stack = ci->stackent;
        pc = ci->pc;
        cipop(mrb);
        JUMP;
      }
      else {
        /* setup environment for calling method */
        proc = ci->proc = MRB_METHOD_PROC(m);
        irep = proc->body.irep;
        pool = irep->pool;
        syms = irep->syms;
        ci->nregs = irep->nregs;
        stack_extend(mrb, (argc < 0 && ci->nregs < 3) ? 3 : ci->nregs);
        pc = irep->iseq;
        JUMP;
      }
    }

    CASE(OP_FSEND) {
      /* A B C  R(A) := fcall(R(A),Syms(B),R(A+1),... ,R(A+C-1)) */
      /* not implemented yet */
      NEXT;
    }

    CASE(OP_CALL) {
      /* A      R(A) := self.call(frame.argc, frame.argv) */
      mrb_callinfo *ci = mrb->c->ci;
      mrb_value recv = mrb->c->stack[0];
      struct RProc *m = mrb_proc_ptr(recv);

      /* replace callinfo */
      ci = mrb->c->ci;
      ci->target_class = MRB_PROC_TARGET_CLASS(m);
      ci->proc = m;
      if (MRB_PROC_ENV_P(m)) {
        mrb_sym mid;
        struct REnv *e = MRB_PROC_ENV(m);

        mid = e->mid;
        if (mid) ci->mid = mid;
        if (!e->stack) {
          e->stack = mrb->c->stack;
        }
      }

      /* prepare stack */
      if (MRB_PROC_CFUNC_P(m)) {
	orgdisflg = mrb->compile_info.disable_jit;
	mrb->compile_info.disable_jit = 1;
	mrb->vmstatus = (void *)&status;
        recv = MRB_PROC_CFUNC(m)(mrb, recv);
	mrb->compile_info.disable_jit = orgdisflg;

        mrb_gc_arena_restore(mrb, ai);
        mrb_gc_arena_shrink(mrb, ai);
        if (mrb->exc) goto L_RAISE;
        /* pop stackpos */
        ci = mrb->c->ci;
        mrb->c->stack = ci->stackent;
        regs[ci->acc] = recv;
        pc = ci->pc;
        cipop(mrb);
        irep = mrb->c->ci->proc->body.irep;
        pool = irep->pool;
        syms = irep->syms;
        JUMP;
      }
      else {
        /* setup environment for calling method */
        proc = m;
        irep = m->body.irep;
        if (!irep) {
          mrb->c->stack[0] = mrb_nil_value();
          goto L_RETURN;
        }
        pool = irep->pool;
        syms = irep->syms;
        ci->nregs = irep->nregs;
        stack_extend(mrb, ci->nregs);
        if (ci->argc < 0) {
          if (irep->nregs > 3) {
            stack_clear(mrb, regs+3, irep->nregs-3);
          }
        }
        else if (ci->argc+2 < irep->nregs) {
          stack_clear(mrb, regs+ci->argc+2, irep->nregs-ci->argc-2);
        }
        if (MRB_PROC_ENV_P(m)) {
          regs[0] = MRB_PROC_ENV(m)->stack[0];
        }
        pc = irep->iseq;
        JUMP;
      }
    }

    CASE(OP_SUPER) {
      /* A C  R(A) := super(R(A+1),... ,R(A+C+1)) */
      int a = GETARG_A(i);
      int n = GETARG_C(i);
      int argc = (n == CALL_MAXARGS) ? -1 : n;
      int bidx = (argc < 0) ? a+2 : a+n+1;
      mrb_method_t m;
      struct RClass *c;
      mrb_callinfo *ci = mrb->c->ci;
      mrb_value recv, blk;
      mrb_sym mid = ci->mid;
      struct RClass* target_class = MRB_PROC_TARGET_CLASS(ci->proc);

      mrb_assert(bidx < ci->nregs);

      if (mid == 0 || !target_class) {
        mrb_value exc = mrb_exc_new_str_lit(mrb, E_NOMETHOD_ERROR, "super called outside of method");
        mrb_exc_set(mrb, exc);
        goto L_RAISE;
      }
      if (target_class->tt == MRB_TT_MODULE) {
        target_class = ci->target_class;
        if (target_class->tt != MRB_TT_ICLASS) {
          mrb_value exc = mrb_exc_new_str_lit(mrb, E_RUNTIME_ERROR, "superclass info lost [mruby limitations]");
          mrb_exc_set(mrb, exc);
          goto L_RAISE;
        }
      }
      recv = regs[0];
      if (!mrb_obj_is_kind_of(mrb, recv, target_class)) {
        mrb_value exc = mrb_exc_new_str_lit(mrb, E_TYPE_ERROR,
                                            "self has wrong type to call super in this context");
        mrb_exc_set(mrb, exc);
        goto L_RAISE;
      }
      blk = regs[bidx];
      if (!mrb_nil_p(blk) && mrb_type(blk) != MRB_TT_PROC) {
        blk = mrb_convert_type(mrb, blk, MRB_TT_PROC, "Proc", "to_proc");
        /* The stack or ci stack might have been reallocated during
           mrb_convert_type(), see #3622 and #3784 */
        regs[bidx] = blk;
        ci = mrb->c->ci;
      }
      c = target_class->super;
      m = mrb_method_search_vm(mrb, &c, mid);
      if (MRB_METHOD_UNDEF_P(m)) {
        mrb_sym missing = mrb_intern_lit(mrb, "method_missing");

        if (mid != missing) {
          c = mrb_class(mrb, recv);
        }
        m = mrb_method_search_vm(mrb, &c, missing);
        if (MRB_METHOD_UNDEF_P(m)) {
          mrb_value args = (argc < 0) ? regs[a+1] : mrb_ary_new_from_values(mrb, n, regs+a+1);
          ERR_PC_SET(mrb, pc);
          mrb_method_missing(mrb, mid, recv, args);
        }
        mid = missing;
        if (argc >= 0) {
          if (a+2 >= ci->nregs) {
            stack_extend(mrb, a+3);
          }
          regs[a+1] = mrb_ary_new_from_values(mrb, n, regs+a+1);
          regs[a+2] = blk;
          argc = -1;
        }
        mrb_ary_unshift(mrb, regs[a+1], mrb_symbol_value(ci->mid));
      }

      if (MRB_DMETHOD_CFUNC_P(m)) {
 	regs[a] = (MRB_DMETHOD_CFUNC(m))(mrb, regs + a, argc);
        mrb_gc_arena_restore(mrb, ai);
        mrb_gc_arena_shrink(mrb, ai);
        if (mrb->exc) {
	  goto L_RAISE;
	}
	  
	NEXT;
      }

      /* push callinfo */
      ci = cipush(mrb);
      ci->mid = mid;
      ci->stackent = mrb->c->stack;
      ci->target_class = c;
      ci->pc = pc + 1;
      ci->argc = argc;

      /* prepare stack */
      mrb->c->stack += a;
      mrb->c->stack[0] = recv;

      if (MRB_METHOD_CFUNC_P(m)) {
        mrb_value v;

	orgdisflg = mrb->compile_info.disable_jit;
	mrb->compile_info.disable_jit = 1;
        ci->nregs = (argc < 0) ? 3 : n+2;
	mrb->vmstatus = (void *)&status;
        v = MRB_METHOD_CFUNC(m)(mrb, recv);
        mrb->compile_info.disable_jit = orgdisflg;

        mrb_gc_arena_restore(mrb, ai);
        if (mrb->exc) goto L_RAISE;
        ci = mrb->c->ci;
        if (!ci->target_class) { /* return from context modifying method (resume/yield) */
          if (ci->acc == CI_ACC_RESUMED) {
            mrb->jmp = prev_jmp;
            return v;
          }
          else {
            mrb_assert(!MRB_PROC_CFUNC_P(ci[-1].proc));
            proc = ci[-1].proc;
            irep = proc->body.irep;
            pool = irep->pool;
            syms = irep->syms;
          }
        }
        mrb->c->stack[0] = v;
        /* pop stackpos */
        mrb->c->stack = ci->stackent;
        pc = ci->pc;
        cipop(mrb);
        JUMP;
      }
      else {
        /* fill callinfo */
        ci->acc = a;

        /* setup environment for calling method */
        proc = ci->proc = MRB_METHOD_PROC(m);
        irep = proc->body.irep;
        pool = irep->pool;
        syms = irep->syms;
        ci->nregs = irep->nregs;
        stack_extend(mrb, (argc < 0 && ci->nregs < 3) ? 3 : ci->nregs);
        pc = irep->iseq;
        JUMP;
      }
    }

    CASE(OP_ARGARY) {
      /* A Bx   R(A) := argument array (16=6:1:5:4) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      int m1 = (bx>>10)&0x3f;
      int r  = (bx>>9)&0x1;
      int m2 = (bx>>4)&0x1f;
      int lv = (bx>>0)&0xf;
      mrb_value *stack;

      if (mrb->c->ci->mid == 0 || mrb->c->ci->target_class == NULL) {
        mrb_value exc;

      L_NOSUPER:
        exc = mrb_exc_new_str_lit(mrb, E_NOMETHOD_ERROR, "super called outside of method");
        mrb_exc_set(mrb, exc);
        goto L_RAISE;
      }
      if (lv == 0) stack = regs + 1;
      else {
        struct REnv *e = uvenv(mrb, lv-1);
        if (!e) goto L_NOSUPER;
        if (MRB_ENV_STACK_LEN(e) <= m1+r+m2+1)
          goto L_NOSUPER;
        stack = e->stack + 1;
      }
      if (r == 0) {
        regs[a] = mrb_ary_new_from_values(mrb, m1+m2, stack);
      }
      else {
        mrb_value *pp = NULL;
        struct RArray *rest;
        int len = 0;

        if (mrb_array_p(stack[m1])) {
          struct RArray *ary = mrb_ary_ptr(stack[m1]);

          pp = ARY_PTR(ary);
          len = (int)ARY_LEN(ary);
        }
        regs[a] = mrb_ary_new_capa(mrb, m1+len+m2);
        rest = mrb_ary_ptr(regs[a]);
        if (m1 > 0) {
          stack_copy(ARY_PTR(rest), stack, m1);
        }
        if (len > 0) {
          stack_copy(ARY_PTR(rest)+m1, pp, len);
        }
        if (m2 > 0) {
          stack_copy(ARY_PTR(rest)+m1+len, stack+m1+1, m2);
        }
        ARY_SET_LEN(rest, m1+len+m2);
      }
      regs[a+1] = stack[m1+r+m2];
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ENTER) {
      /* Ax             arg setup according to flags (23=5:5:1:5:5:1:1) */
      /* number of optional arguments times OP_JMP should follow */
      mrb_aspec ax = GETARG_Ax(i);
      int m1 = MRB_ASPEC_REQ(ax);
      int o  = MRB_ASPEC_OPT(ax);
      int r  = MRB_ASPEC_REST(ax);
      int m2 = MRB_ASPEC_POST(ax);
      /* unused
      int k  = MRB_ASPEC_KEY(ax);
      int kd = MRB_ASPEC_KDICT(ax);
      int b  = MRB_ASPEC_BLOCK(ax);
      */
      int argc = mrb->c->ci->argc;
      int oargc = mrb->c->ci->argc;
      mrb_value *argv = regs+1;
      mrb_value *argv0 = argv;
      int len = m1 + o + r + m2;
      mrb_value *blk = &argv[argc < 0 ? 1 : argc];

      if (argc < 0) {
        struct RArray *ary = mrb_ary_ptr(regs[1]);
        argv = ARY_PTR(ary);
        argc = (int)ARY_LEN(ary);
        mrb_gc_protect(mrb, regs[1]);
      }
      if (mrb->c->ci->proc && MRB_PROC_STRICT_P(mrb->c->ci->proc)) {
        if (argc >= 0) {
          if (argc < m1 + m2 || (r == 0 && argc > len)) {
            argnum_error(mrb, m1+m2);
            goto L_RAISE;
          }
        }
      }
      else if (len > 1 && argc == 1 && mrb_array_p(argv[0])) {
        mrb_gc_protect(mrb, argv[0]);
        argc = (int)RARRAY_LEN(argv[0]);
        argv = RARRAY_PTR(argv[0]);
      }
      if (argc < len) {
        int mlen = m2;
        if (argc < m1+m2) {
          if (m1 < argc)
            mlen = argc - m1;
          else
            mlen = 0;
        }
        regs[len+1] = *blk; /* move block */
        SET_NIL_VALUE(regs[argc+1]);
        if (argv0 != argv) {
          value_move(&regs[1], argv, argc-mlen); /* m1 + o */
        }
        if (argc < m1) {
          stack_clear(mrb, &regs[argc+1], m1-argc);
        }
        if (mlen) {
          value_move(&regs[len-m2+1], &argv[argc-mlen], mlen);
        }
        if (mlen < m2) {
          stack_clear(mrb, &regs[len-m2+mlen+1], m2-mlen);
        }
        if (r) {
          regs[m1+o+1] = mrb_ary_new_capa(mrb, 0);
        }
        if (o == 0 || argc < m1+m2) pc++;
        else
          pc += argc - m1 - m2 + 1;
      }
      else {
        int rnum = 0;
        if (argv0 != argv) {
          regs[len+1] = *blk; /* move block */
          value_move(&regs[1], argv, m1+o);
        }
        if (r) {
          rnum = argc-m1-o-m2;
	  //printf("%d %x\n", rnum, irep);
	  //disasm_irep(mrb, irep);
	  if (rnum == 1 && oargc >= 0) {
	    int ipos = 0;
	    mrb_irep *nirep = (mrb_irep *)((uintptr_t)mrb + mrb_fixnum(irep->pool[ipos]));
	    struct RProc *p;

	    if (nirep == (mrb_irep *)mrb) {
	      int i;
	      mrb_irep *cirep = mrb_add_irep(mrb);
	      *cirep = *irep;
	      cirep->jit_entry_tab = (mrbjit_codetab *)mrb_calloc(mrb, irep->ilen, sizeof(mrbjit_codetab));
	      for (i = 0; i < cirep->ilen; i++) {
		cirep->jit_entry_tab[i].size = 2;
		cirep->jit_entry_tab[i].body = 
		  (mrbjit_code_info *)mrb_calloc(mrb, 2, sizeof(mrbjit_code_info));
	      }
	      cirep->prof_info = (int *)mrb_calloc(mrb, 1, sizeof(int)*irep->ilen);
	      if (mrb_patch_irep_var2fix(mrb, cirep, m1 + o + 1)) {
		if (irep->shared_lambda == 1) {
		  p = get_local_proc(mrb, cirep);
		}
		else {
		  p = mrb_proc_new(mrb, cirep);
		}
		p->flags = proc->flags;
		p->body.irep->refcnt++;
		p->e.env = proc->e.env;
		p->upper = proc->upper;
		irep->pool[ipos] = mrb_fixnum_value((uintptr_t)cirep - (uintptr_t)mrb);
		mrb->c->ci->proc = proc = p;
		irep = cirep;
		pc = cirep->iseq;
		mrb->c->ci->prev_pc = NULL;
		//assert(p->env == NULL || p->env->cioff >= 0);
	      }
	      else {
		regs[m1+o+1] = mrb_ary_new_from_values(mrb, rnum, argv+m1+o);
	      }
	    }
	    else {
	      if (irep->shared_lambda == 1) {
		p = get_local_proc(mrb, nirep);
	      }
	      else {
		p = mrb_proc_new(mrb, nirep);
	      }
	      p->flags = proc->flags;
	      p->body.irep->refcnt++;
	      p->e.env = proc->e.env;
	      p->upper = proc->upper;
	      mrb->c->ci->proc = proc = p;
	      irep = nirep;
	      pc = nirep->iseq;
	      mrb->c->ci->prev_pc = NULL;
	      //	      assert(p->env == NULL || p->env->cioff >= 0);
	    }
	  }
	  else {
	    regs[m1+o+1] = mrb_ary_new_from_values(mrb, rnum, argv+m1+o);
	  }
        }
        if (m2) {
          if (argc-m2 > m1) {
            value_move(&regs[m1+o+r+1], &argv[m1+o+rnum], m2);
          }
        }
        if (argv0 == argv) {
          regs[len+1] = *blk; /* move block */
        }
        pc += o + 1;
      }
      mrb->c->ci->argc = len;
      /* clear local (but non-argument) variables */
      if ((int)(irep->nlocals-len-2) > 0) {
        stack_clear(mrb, &regs[len+2], irep->nlocals-len-2);
      }
      JUMP;
    }

    CASE(OP_KARG) {
      /* A B C          R(A) := kdict[Syms(B)]; if C kdict.rm(Syms(B)) */
      /* if C == 2; raise unless kdict.empty? */
      /* OP_JMP should follow to skip init code */
      NEXT;
    }

    CASE(OP_KDICT) {
      /* A C            R(A) := kdict */
      NEXT;
    }

    L_RETURN:
      i = MKOP_AB(OP_RETURN, GETARG_A(i), OP_R_NORMAL);
      /* fall through */
    CASE(OP_RETURN) {
      /* A B     return R(A) (B=normal,in-block return/break) */
      mrb_callinfo *ci;

#define ecall_adjust() do {\
  ptrdiff_t cioff = ci - mrb->c->cibase;\
  ecall(mrb);\
  ci = mrb->c->cibase + cioff;\
} while (0)

      ci = mrb->c->ci;
      if (ci->mid) {
        mrb_value blk;

        if (ci->argc < 0) {
          blk = regs[2];
        }
        else {
          blk = regs[ci->argc+1];
        }
        if (mrb_type(blk) == MRB_TT_PROC) {
          struct RProc *p = mrb_proc_ptr(blk);

          if (!MRB_PROC_STRICT_P(p) &&
              ci > mrb->c->cibase && MRB_PROC_ENV(p) == ci[-1].env) {
            p->flags |= MRB_PROC_ORPHAN;
          }
        }
      }

      if (mrb->exc) {
        mrb_callinfo *ci0;

      L_RAISE:
	//disasm_once(mrb, irep, *pc);
	/* Avoid Instruction JIT compile that happend exception */
	if (mrb->compile_info.code_base && !mrb->compile_info.disable_jit) {
	  mrbjit_gen_exit2(mrb->compile_info.code_base, mrb, irep, &pc, &status, NULL);
	  mrb->compile_info.code_base = NULL;
	}

        ci0 = ci = mrb->c->ci;
        if (ci == mrb->c->cibase) {
          if (ci->ridx == 0) goto L_FTOP;
          goto L_RESCUE;
        }
        while (ci[0].ridx == ci[-1].ridx) {
          cipop(mrb);
          mrb->c->stack = ci->stackent;
          if (ci->acc == CI_ACC_SKIP && prev_jmp) {
            mrb->jmp = prev_jmp;
	    mrb->compile_info.disable_jit = orgdisflg;
            MRB_THROW(prev_jmp);
          }
          ci = mrb->c->ci;
          if (ci == mrb->c->cibase) {
            if (ci->ridx == 0) {
            L_FTOP:             /* fiber top */
              if (mrb->c == mrb->root_c) {
                mrb->c->stack = mrb->c->stbase;
                goto L_STOP;
              }
              else {
                struct mrb_context *c = mrb->c;

                while (c->eidx > ci->epos) {
                  ecall_adjust();
                }
                if (c->fib) {
                  mrb_write_barrier(mrb, (struct RBasic*)c->fib);
                }
                mrb->c->status = MRB_FIBER_TERMINATED;
                mrb->c = c->prev;
                c->prev = NULL;
                goto L_RAISE;
              }
            }
	    mrb->compile_info.disable_jit = orgdisflg;
            break;
          }
          /* call ensure only when we skip this callinfo */
          if (ci[0].ridx == ci[-1].ridx) {
            while (mrb->c->eidx > ci->epos) {
              ecall_adjust();
            }
          }
        }

      L_RESCUE:
	if (mrb->compile_info.code_base) {
	  mrbjit_gen_exit2(mrb->compile_info.code_base, mrb, irep, &pc, &status, NULL);
	  mrb->compile_info.code_base = NULL;
	}
        if (ci->ridx == 0) goto L_STOP;
	if (ci->acc > 0) {
	  mrb->compile_info.disable_jit = orgdisflg;
	}
        proc = ci->proc;
        irep = proc->body.irep;
        pool = irep->pool;
        syms = irep->syms;
        if (ci < ci0) {
          mrb->c->stack = ci[1].stackent;
        }
	mrb->c->ci->prev_pc = NULL;
        stack_extend(mrb, irep->nregs);
        pc = mrb->c->rescue[--ci->ridx];
      }
      else {
        int acc;
        mrb_value v;
        struct RProc *dst;

        ci = mrb->c->ci;
        v = regs[GETARG_A(i)];
        mrb_gc_protect(mrb, v);
        switch (GETARG_B(i)) {
        case OP_R_RETURN:
          /* Fall through to OP_R_NORMAL otherwise */
          if (ci->acc >=0 && MRB_PROC_ENV_P(proc) && !MRB_PROC_STRICT_P(proc)) {
            mrb_callinfo *cibase = mrb->c->cibase;
            dst = top_proc(mrb, proc);

            if (MRB_PROC_ENV_P(dst)) {
              struct REnv *e = MRB_PROC_ENV(dst);

              if (!MRB_ENV_STACK_SHARED_P(e) || e->cxt != mrb->c) {
                localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
                goto L_RAISE;
              }
            }
            while (cibase <= ci && ci->proc->body.irep->org_iseq != dst->body.irep->org_iseq) {
              if (ci->acc < 0) {
                localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
                goto L_RAISE;
              }
              ci--;
            }
            if (ci <= cibase) {
              localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
              goto L_RAISE;
            }
            break;
          }
        case OP_R_NORMAL:
        NORMAL_RETURN:
          if (ci == mrb->c->cibase) {
            struct mrb_context *c;

            if (!mrb->c->prev) { /* toplevel return */
              localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
              goto L_RAISE;
            }
            if (mrb->c->prev->ci == mrb->c->prev->cibase) {
              mrb_value exc = mrb_exc_new_str_lit(mrb, E_FIBER_ERROR, "double resume");
              mrb_exc_set(mrb, exc);
              goto L_RAISE;
            }
            while (mrb->c->eidx > 0) {
              ecall(mrb);
            }
            /* automatic yield at the end */
            c = mrb->c;
            c->status = MRB_FIBER_TERMINATED;
            mrb->c = c->prev;
            c->prev = NULL;
            mrb->c->status = MRB_FIBER_RUNNING;
            ci = mrb->c->ci;
          }
          break;
        case OP_R_BREAK:
          if (MRB_PROC_STRICT_P(proc)) goto NORMAL_RETURN;
          if (MRB_PROC_ORPHAN_P(proc)) { 
            mrb_value exc;

          L_BREAK_ERROR:
            exc = mrb_exc_new_str_lit(mrb, E_LOCALJUMP_ERROR,
                                      "break from proc-closure");
            mrb_exc_set(mrb, exc);
            goto L_RAISE;
          }
          if (!MRB_PROC_ENV_P(proc) || !MRB_ENV_STACK_SHARED_P(MRB_PROC_ENV(proc))) {
            goto L_BREAK_ERROR;
          }
          else {
            struct REnv *e = MRB_PROC_ENV(proc);

            if (e == mrb->c->cibase->env && proc->body.irep->org_iseq != mrb->c->cibase->proc->body.irep->org_iseq) {
              goto L_BREAK_ERROR;
            }
            if (e->cxt != mrb->c) {
              goto L_BREAK_ERROR;
            }
          }
          while (mrb->c->eidx > mrb->c->ci->epos) {
            ecall_adjust();
          }
          /* break from fiber block */
          if (ci == mrb->c->cibase && ci->pc) {
            struct mrb_context *c = mrb->c;

            mrb->c = c->prev;
            c->prev = NULL;
            ci = mrb->c->ci;
          }
          if (ci->acc < 0) {
            mrb_gc_arena_restore(mrb, ai);
            mrb->c->vmexec = FALSE;
            mrb->exc = (struct RObject*)break_new(mrb, proc, v);
            mrb->jmp = prev_jmp;
            MRB_THROW(prev_jmp);
          }
          if (FALSE) {
          L_BREAK:
            v = ((struct RBreak*)mrb->exc)->val;
            proc = ((struct RBreak*)mrb->exc)->proc;
            mrb->exc = NULL;
            ci = mrb->c->ci;
          }
          mrb->c->stack = ci->stackent;
          proc = proc->upper;
          while (mrb->c->cibase < ci &&  ci[-1].proc->body.irep->org_iseq != proc->body.irep->org_iseq) {
            if (ci[-1].acc == CI_ACC_SKIP) {
              while (ci < mrb->c->ci) {
                cipop(mrb);
              }
              goto L_BREAK_ERROR;
            }
            ci--;
          }
          if (ci == mrb->c->cibase) {
            goto L_BREAK_ERROR;
          }
          break;
        default:
          /* cannot happen */
          break;
        }
        while (ci < mrb->c->ci) {
          cipop(mrb);
        }
        ci[0].ridx = ci[-1].ridx;
        while (mrb->c->eidx > ci->epos) {
          ecall_adjust();
        }
        if (mrb->c->vmexec && !ci->target_class) {
          mrb_gc_arena_restore(mrb, ai);
          mrb->c->vmexec = FALSE;
          mrb->jmp = prev_jmp;
          return v;
        }
        acc = ci->acc;
        mrb->c->stack = ci->stackent;
        cipop(mrb);
        if (acc == CI_ACC_SKIP || acc == CI_ACC_DIRECT) {
          mrb_gc_arena_restore(mrb, ai);
          mrb->jmp = prev_jmp;
          return v;
        }
        pc = ci->pc;
        ci = mrb->c->ci;
        DEBUG(fprintf(stderr, "from :%s\n", mrb_sym2name(mrb, ci->mid)));
        proc = mrb->c->ci->proc;
        irep = proc->body.irep;
        pool = irep->pool;
        syms = irep->syms;

        regs[acc] = v;
        mrb_gc_arena_restore(mrb, ai);
      }
      JUMP;
    }

    CASE(OP_TAILCALL) {
      /* A B C  return call(R(A),Syms(B),R(A+1),... ,R(A+C+1)) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int n = GETARG_C(i);
      mrb_method_t m;
      struct RClass *c;
      mrb_callinfo *ci;
      mrb_value recv;
      mrb_sym mid = syms[b];


      recv = regs[a];
      if (n == CALL_MAXARGS) {
	SET_NIL_VALUE(regs[a + 2]);
      }
      else {
	SET_NIL_VALUE(regs[a + n+1]);
      }

      c = mrb_class(mrb, recv);
      m = mrb_method_search_vm(mrb, &c, mid);
      if (MRB_METHOD_UNDEF_P(m)) {
        mrb_value sym = mrb_symbol_value(mid);
        mrb_sym missing = mrb_intern_lit(mrb, "method_missing");
        m = mrb_method_search_vm(mrb, &c, missing);
        if (MRB_METHOD_UNDEF_P(m)) {
          mrb_value args;

          if (n == CALL_MAXARGS) {
            args = regs[a+1];
          }
          else {
            args = mrb_ary_new_from_values(mrb, n, regs+a+1);
          }
          ERR_PC_SET(mrb, pc);
          mrb_method_missing(mrb, mid, recv, args);
        }
        mid = missing;
        if (n == CALL_MAXARGS) {
          mrb_ary_unshift(mrb, regs[a+1], sym);
        }
        else {
          value_move(regs+a+2, regs+a+1, ++n);
          regs[a+1] = sym;
        }
      }

      if (mrb->c->ci->env && mrb->c->ci->proc->body.irep->shared_lambda != 1) {
	struct REnv *e = mrb->c->ci->env;
	size_t len = (size_t)MRB_ENV_STACK_LEN(e);
	mrb_value *p = (mrb_value *)mrb_malloc(mrb, sizeof(mrb_value)*len);

	MRB_ENV_UNSHARE_STACK(e);
	if (len > 0) {
	  stack_copy(p, e->stack, len);
	}
	e->stack = p;
	mrb_write_barrier(mrb, (struct RBasic *)e);
      }

      /* replace callinfo */
      ci = mrb->c->ci;
      ci->mid = mid;
      ci->target_class = c;
      ci->env = 0;
      proc = ci->proc = MRB_METHOD_PROC(m);
      if (n == CALL_MAXARGS) {
        ci->argc = -1;
      }
      else {
        ci->argc = n;
      }

      /* move stack */
      value_move(regs, &regs[a], n+2);

      if (MRB_METHOD_CFUNC_P(m)) {
        mrb_value v;
	orgdisflg = mrb->compile_info.disable_jit;
	mrb->compile_info.disable_jit = 1;
	mrb->vmstatus = (void *)&status;
	v = MRB_METHOD_CFUNC(m)(mrb, recv);
        mrb->c->stack[0] = v;
        mrb->compile_info.disable_jit = orgdisflg;
        mrb_gc_arena_restore(mrb, ai);
	i = MKOP_AB(OP_RETURN, 0, OP_R_NORMAL);
        goto L_RETURN;
      }
      else {
        /* setup environment for calling method */
        struct RProc *p = MRB_METHOD_PROC(m);
        irep = p->body.irep;
        pool = irep->pool;
        syms = irep->syms;
        ci->nregs = irep->nregs;
        if (ci->argc < 0) {
          stack_extend(mrb, (irep->nregs < 3) ? 3 : irep->nregs);
        }
        else {
          stack_extend(mrb, irep->nregs);
        }
        pc = irep->iseq;
      }
      JUMP;
    }

    CASE(OP_BLKPUSH) {
      /* A Bx   R(A) := block (16=6:1:5:4) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      int m1 = (bx>>10)&0x3f;
      int r  = (bx>>9)&0x1;
      int m2 = (bx>>4)&0x1f;
      int lv = (bx>>0)&0xf;
      mrb_value *stack;

      if (lv == 0) stack = regs + 1;
      else {
        struct REnv *e = uvenv(mrb, lv-1);
        if (!e || (!MRB_ENV_STACK_SHARED_P(e) && e->mid == 0) ||
            MRB_ENV_STACK_LEN(e) <= m1+r+m2+1) {
          localjump_error(mrb, LOCALJUMP_ERROR_YIELD);
          goto L_RAISE;
        }
        stack = e->stack + 1;
      }
      if (mrb_nil_p(stack[m1+r+m2])) {
        localjump_error(mrb, LOCALJUMP_ERROR_YIELD);
        goto L_RAISE;
      }
      regs[a] = stack[m1+r+m2];
      NEXT;
    }

#define TYPES2(a,b) ((((uint16_t)(a))<<8)|(((uint16_t)(b))&0xff))
#define OP_MATH_BODY(op,v1,v2) do {\
  v1(regs[a]) = v1(regs[a]) op v2(regs[a+1]);\
} while(0)

    CASE(OP_ADD) {
      /* A B C  R(A) := R(A)+R(A+1) (Syms[B]=:+,C=1)*/
      int a = GETARG_A(i);

      /* need to check if op is overridden */
      switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
        {
          mrb_int x, y, z;
          mrb_value *regs_a = regs + a;

          x = mrb_fixnum(regs_a[0]);
          y = mrb_fixnum(regs_a[1]);
          if (mrb_int_add_overflow(x, y, &z)) {
#ifndef MRB_WITHOUT_FLOAT
            SET_FLOAT_VALUE(mrb, regs_a[0], (mrb_float)x + (mrb_float)y);
            break;
#endif
          }
          SET_INT_VALUE(regs[a], z);
        }
        break;
#ifndef MRB_WITHOUT_FLOAT
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
        {
          mrb_int x = mrb_fixnum(regs[a]);
          mrb_float y = mrb_float(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x + y);
        }
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          mrb_int y = mrb_fixnum(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], x + y);
        }
#else
        OP_MATH_BODY(+,mrb_float,mrb_fixnum);
#endif
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          mrb_float y = mrb_float(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], x + y);
        }
#else
        OP_MATH_BODY(+,mrb_float,mrb_float);
#endif
        break;
#endif
      case TYPES2(MRB_TT_STRING,MRB_TT_STRING):
        regs[a] = mrb_str_plus(mrb, regs[a], regs[a+1]);
        break;
      default:
        goto L_SEND;
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_SUB) {
      /* A B C  R(A) := R(A)-R(A+1) (Syms[B]=:-,C=1)*/
      int a = GETARG_A(i);

      /* need to check if op is overridden */
      switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
        {
          mrb_int x, y, z;

          x = mrb_fixnum(regs[a]);
          y = mrb_fixnum(regs[a+1]);
          if (mrb_int_sub_overflow(x, y, &z)) {
#ifndef MRB_WITHOUT_FLOAT
            SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x - (mrb_float)y);
            break;
#endif
          }
          SET_INT_VALUE(regs[a], z);
        }
        break;
#ifndef MRB_WITHOUT_FLOAT
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
        {
          mrb_int x = mrb_fixnum(regs[a]);
          mrb_float y = mrb_float(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x - y);
        }
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          mrb_int y = mrb_fixnum(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], x - y);
        }
#else
        OP_MATH_BODY(-,mrb_float,mrb_fixnum);
#endif
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          mrb_float y = mrb_float(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], x - y);
        }
#else
        OP_MATH_BODY(-,mrb_float,mrb_float);
#endif
        break;
#endif
      default:
        goto L_SEND;
      }
      NEXT;
    }

    CASE(OP_MUL) {
      /* A B C  R(A) := R(A)*R(A+1) (Syms[B]=:*,C=1)*/
      int a = GETARG_A(i);

      /* need to check if op is overridden */
      switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
        {
          mrb_int x, y, z;

          x = mrb_fixnum(regs[a]);
          y = mrb_fixnum(regs[a+1]);
          if (mrb_int_mul_overflow(x, y, &z)) {
#ifndef MRB_WITHOUT_FLOAT
            SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x * (mrb_float)y);
            break;
#endif
          }
          SET_INT_VALUE(regs[a], z);
        }
        break;
#ifndef MRB_WITHOUT_FLOAT
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
        {
          mrb_int x = mrb_fixnum(regs[a]);
          mrb_float y = mrb_float(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x * y);
        }
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          mrb_int y = mrb_fixnum(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], x * y);
        }
#else
        OP_MATH_BODY(*,mrb_float,mrb_fixnum);
#endif
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          mrb_float y = mrb_float(regs[a+1]);
          SET_FLOAT_VALUE(mrb, regs[a], x * y);
        }
#else
        OP_MATH_BODY(*,mrb_float,mrb_float);
#endif
        break;
#endif
      default:
        goto L_SEND;
      }
      NEXT;
    }

    CASE(OP_DIV) {
      /* A B C  R(A) := R(A)/R(A+1) (Syms[B]=:/,C=1)*/
      int a = GETARG_A(i);
#ifndef MRB_WITHOUT_FLOAT
      double x, y, f;
#endif

      /* need to check if op is overridden */
      switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
#ifdef MRB_WITHOUT_FLOAT
        {
          mrb_int x = mrb_fixnum(regs[a]);
          mrb_int y = mrb_fixnum(regs[a+1]);
          SET_INT_VALUE(regs[a], y ? x / y : 0);
        }
        break;
#else
        x = (mrb_float)mrb_fixnum(regs[a]);
        y = (mrb_float)mrb_fixnum(regs[a+1]);
        break;
      case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
        x = (mrb_float)mrb_fixnum(regs[a]);
        y = mrb_float(regs[a+1]);
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
        x = mrb_float(regs[a]);
        y = (mrb_float)mrb_fixnum(regs[a+1]);
        break;
      case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
        x = mrb_float(regs[a]);
        y = mrb_float(regs[a+1]);
        break;
#endif
      default:
        goto L_SEND;
      }

#ifndef MRB_WITHOUT_FLOAT
      if (y == 0) {
        if (x > 0) f = INFINITY;
        else if (x < 0) f = -INFINITY;
        else /* if (x == 0) */ f = NAN;
      }
      else {
        f = x / y;
      }
      SET_FLOAT_VALUE(mrb, regs[a], f);
#endif
      NEXT;
    }

    CASE(OP_ADDI) {
      /* A B C  R(A) := R(A)+C (Syms[B]=:+)*/
      int a = GETARG_A(i);

      /* need to check if + is overridden */
      switch (mrb_type(regs[a])) {
      case MRB_TT_FIXNUM:
        {
          mrb_int x = mrb_fixnum(regs[a]);
          mrb_int y = GETARG_C(i);
          mrb_int z;

          if (mrb_int_add_overflow(x, y, &z)) {
#ifndef MRB_WITHOUT_FLOAT
            SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x + (mrb_float)y);
            break;
#endif
          }
          SET_INT_VALUE(regs[a], z);
        }
        break;
#ifndef MRB_WITHOUT_FLOAT
      case MRB_TT_FLOAT:
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          SET_FLOAT_VALUE(mrb, regs[a], x + GETARG_C(i));
        }
#else
        mrb_float(regs[a]) += GETARG_C(i);
#endif
        break;
#endif
      default:
        SET_INT_VALUE(regs[a+1], GETARG_C(i));
        i = MKOP_ABC(OP_SEND, a, GETARG_B(i), 1);
        goto L_SEND;
      }
      NEXT;
    }

    CASE(OP_SUBI) {
      /* A B C  R(A) := R(A)-C (Syms[B]=:-)*/
      int a = GETARG_A(i);
      mrb_value *regs_a = regs + a;

      /* need to check if + is overridden */
      switch (mrb_type(regs_a[0])) {
      case MRB_TT_FIXNUM:
        {
          mrb_int x = mrb_fixnum(regs_a[0]);
          mrb_int y = GETARG_C(i);
          mrb_int z;

          if (mrb_int_sub_overflow(x, y, &z)) {
#ifndef MRB_WITHOUT_FLOAT
            SET_FLOAT_VALUE(mrb, regs_a[0], (mrb_float)x - (mrb_float)y);
            break;
#endif
          }
          SET_INT_VALUE(regs_a[0], z);
        }
        break;
#ifndef MRB_WITHOUT_FLOAT
      case MRB_TT_FLOAT:
#ifdef MRB_WORD_BOXING
        {
          mrb_float x = mrb_float(regs[a]);
          SET_FLOAT_VALUE(mrb, regs[a], x - GETARG_C(i));
        }
#else
        mrb_float(regs_a[0]) -= GETARG_C(i);
#endif
        break;
#endif
      default:
        SET_INT_VALUE(regs_a[1], GETARG_C(i));
        i = MKOP_ABC(OP_SEND, a, GETARG_B(i), 1);
        goto L_SEND;
      }
      NEXT;
    }

#define OP_CMP_BODY(op,v1,v2) (v1(regs[a]) op v2(regs[a+1]))

#ifdef MRB_WITHOUT_FLOAT
#define OP_CMP(op) do {\
  int result;\
  /* need to check if - is overridden */\
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {\
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):\
    result = OP_CMP_BODY(op,mrb_fixnum,mrb_fixnum);\
    break;\
  default:\
    goto L_SEND;\
  }\
  if (result) {\
    SET_TRUE_VALUE(regs[a]);\
  }\
  else {\
    SET_FALSE_VALUE(regs[a]);\
  }\
} while(0)
#else
#define OP_CMP(op) do {\
  int result;\
  /* need to check if - is overridden */\
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {\
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):\
    result = OP_CMP_BODY(op,mrb_fixnum,mrb_fixnum);\
    break;\
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):\
    result = OP_CMP_BODY(op,mrb_fixnum,mrb_float);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):\
    result = OP_CMP_BODY(op,mrb_float,mrb_fixnum);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):\
    result = OP_CMP_BODY(op,mrb_float,mrb_float);\
    break;\
  default:\
    goto L_SEND;\
  }\
  if (result) {\
    SET_TRUE_VALUE(regs[a]);\
  }\
  else {\
    SET_FALSE_VALUE(regs[a]);\
  }\
} while(0)
#endif

    CASE(OP_EQ) {
      /* A B C  R(A) := R(A)==R(A+1) (Syms[B]=:==,C=1)*/
      int a = GETARG_A(i);

      OP_CMP(==);
      NEXT;
    }

    CASE(OP_LT) {
      /* A B C  R(A) := R(A)<R(A+1) (Syms[B]=:<,C=1)*/
      int a = GETARG_A(i);
      OP_CMP(<);
      NEXT;
    }

    CASE(OP_LE) {
      /* A B C  R(A) := R(A)<=R(A+1) (Syms[B]=:<=,C=1)*/
      int a = GETARG_A(i);
      OP_CMP(<=);
      NEXT;
    }

    CASE(OP_GT) {
      /* A B C  R(A) := R(A)>R(A+1) (Syms[B]=:>,C=1)*/
      int a = GETARG_A(i);
      OP_CMP(>);
      NEXT;
    }

    CASE(OP_GE) {
      /* A B C  R(A) := R(A)>=R(A+1) (Syms[B]=:>=,C=1)*/
      int a = GETARG_A(i);
      OP_CMP(>=);
      NEXT;
    }

    CASE(OP_ARRAY) {
      /* A B C          R(A) := ary_new(R(B),R(B+1)..R(B+C)) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      mrb_value v = mrb_ary_new_from_values(mrb, c, &regs[b]);
      regs[a] = v;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ARYCAT) {
      /* A B            mrb_ary_concat(R(A),R(B)) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      mrb_value splat = mrb_ary_splat(mrb, regs[b]);
      mrb_ary_concat(mrb, regs[a], splat);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ARYPUSH) {
      /* A B            R(A).push(R(B)) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      mrb_ary_push(mrb, regs[a], regs[b]);
      NEXT;
    }

    CASE(OP_AREF) {
      /* A B C          R(A) := R(B)[C] */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      mrb_value v = regs[b];

      if (!mrb_array_p(v)) {
        if (c == 0) {
          regs[a] = v;
        }
        else {
          SET_NIL_VALUE(regs[a]);
        }
      }
      else {
        v = mrb_ary_ref(mrb, v, c);
        regs[a] = v;
      }
      NEXT;
    }

    CASE(OP_ASET) {
      /* A B C          R(B)[C] := R(A) */
      int a = GETARG_A(i);
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      mrb_ary_set(mrb, regs[b], c, regs[a]);
      NEXT;
    }

    CASE(OP_APOST) {
      /* A B C  *R(A),R(A+1)..R(A+C) := R(A) */
      int a = GETARG_A(i);
      mrb_value v = regs[a];
      int pre  = GETARG_B(i);
      int post = GETARG_C(i);
      struct RArray *ary;
      int len, idx;

      if (!mrb_array_p(v)) {
        v = mrb_ary_new_from_values(mrb, 1, &regs[a]);
      }
      ary = mrb_ary_ptr(v);
      len = (int)ARY_LEN(ary);
      if (len > pre + post) {
        v = mrb_ary_new_from_values(mrb, len - pre - post, ARY_PTR(ary)+pre);
        regs[a++] = v;
        while (post--) {
          regs[a++] = ARY_PTR(ary)[len-post-1];
        }
      }
      else {
        v = mrb_ary_new_capa(mrb, 0);
        regs[a++] = v;
        for (idx=0; idx+pre<len; idx++) {
          regs[a+idx] = ARY_PTR(ary)[pre+idx];
        }
        while (idx < post) {
          SET_NIL_VALUE(regs[a+idx]);
          idx++;
        }
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_STRING) {
      /* A Bx           R(A) := str_new(Lit(Bx)) */
      mrb_int a = GETARG_A(i);
      mrb_int bx = GETARG_Bx(i);
      mrb_value str = mrb_str_dup(mrb, pool[bx]);

      regs[a] = str;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_STRCAT) {
      /* A B    R(A).concat(R(B)) */
      mrb_int a = GETARG_A(i);
      mrb_int b = GETARG_B(i);

      mrb_str_concat(mrb, regs[a], regs[b]);
      NEXT;
    }

    CASE(OP_HASH) {
      /* A B C   R(A) := hash_new(R(B),R(B+1)..R(B+C)) */
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      int lim = b+c*2;
      mrb_value hash = mrb_hash_new_capa(mrb, c);

      while (b < lim) {
        mrb_hash_set(mrb, hash, regs[b], regs[b+1]);
        b+=2;
      }
      regs[GETARG_A(i)] = hash;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_LAMBDA) {
      /* A b c  R(A) := lambda(SEQ[b],c) (b:c = 14:2) */
      struct RProc *p;
      int a = GETARG_A(i);
      int b = GETARG_b(i);
      int c = GETARG_c(i);
      mrb_irep *nirep = irep->reps[b];

      if (nirep->shared_lambda == 1 && !(c & OP_L_CAPTURE)) {
	p = get_local_proc(mrb, nirep);
	p->e.env->stack = mrb->c->stack;
	p->e.env->c = (struct RClass*)mrb->c->ci->proc->e.env;
      }
      else {
	if (c & OP_L_CAPTURE) {
	  p = mrb_closure_new(mrb, nirep);
	}
	else {
	  p = mrb_proc_new(mrb, nirep);
	  p->flags |= MRB_PROC_SCOPE;
	}
      }
      if (c & OP_L_STRICT) p->flags |= MRB_PROC_STRICT;
      regs[a] = mrb_obj_value(p);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_OCLASS) {
      /* A      R(A) := ::Object */
      regs[GETARG_A(i)] = mrb_obj_value(mrb->object_class);
      NEXT;
    }

    CASE(OP_CLASS) {
      /* A B    R(A) := newclass(R(A),Syms(B),R(A+1)) */
      struct RClass *c = 0, *baseclass;
      int a = GETARG_A(i);
      mrb_value base, super;
      mrb_sym id = syms[GETARG_B(i)];

      base = regs[a];
      super = regs[a+1];
      if (mrb_nil_p(base)) {
        baseclass = MRB_PROC_TARGET_CLASS(mrb->c->ci->proc);
        base = mrb_obj_value(baseclass);
      }
      c = mrb_vm_define_class(mrb, base, super, id);
      regs[a] = mrb_obj_value(c);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_MODULE) {
      /* A B            R(A) := newmodule(R(A),Syms(B)) */
      struct RClass *c = 0, *baseclass;
      int a = GETARG_A(i);
      mrb_value base;
      mrb_sym id = syms[GETARG_B(i)];

      base = regs[a];
      if (mrb_nil_p(base)) {
        baseclass = MRB_PROC_TARGET_CLASS(mrb->c->ci->proc);
        base = mrb_obj_value(baseclass);
      }
      c = mrb_vm_define_module(mrb, base, id);
      regs[a] = mrb_obj_value(c);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_EXEC) {
      /* A Bx   R(A) := blockexec(R(A),SEQ[Bx]) */
      int a = GETARG_A(i);
      int bx = GETARG_Bx(i);
      mrb_callinfo *ci;
      mrb_value recv = regs[a];
      struct RProc *p;
      mrb_irep *nirep = irep->reps[bx];

      /* prepare closure */
      p = mrb_proc_new(mrb, nirep);
      p->c = NULL;
      mrb_field_write_barrier(mrb, (struct RBasic*)p, (struct RBasic*)proc);
      MRB_PROC_SET_TARGET_CLASS(p, mrb_class_ptr(recv));
      p->flags |= MRB_PROC_SCOPE;

      /* prepare call stack */
      ci = cipush(mrb);
      ci->pc = pc + 1;
      ci->acc = a;
      ci->mid = 0;
      ci->stackent = mrb->c->stack;
      ci->argc = 0;
      ci->target_class = mrb_class_ptr(recv);

      /* prepare stack */
      mrb->c->stack += a;

      /* setup block to call */
      proc = ci->proc = p;

      irep = p->body.irep;
      pool = irep->pool;
      syms = irep->syms;
      ci->nregs = irep->nregs;
      stack_extend(mrb, ci->nregs);
      stack_clear(mrb, regs+1, ci->nregs-1);
      pc = irep->iseq;
      JUMP;
    }

    CASE(OP_METHOD) {
      /* A B            R(A).newmethod(Syms(B),R(A+1)) */
      int a = GETARG_A(i);
      struct RClass *c = mrb_class_ptr(regs[a]);
      struct RProc *p = mrb_proc_ptr(regs[a+1]);
      mrb_method_t m;

      mrb->vmstatus = (void *)&status;
      MRB_METHOD_FROM_PROC(m, p);
      mrb_define_method_raw(mrb, c, syms[GETARG_B(i)], m);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_SCLASS) {
      /* A B    R(A) := R(B).singleton_class */
      int a = GETARG_A(i);
      int b = GETARG_B(i);

      regs[a] = mrb_singleton_class(mrb, regs[b]);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_TCLASS) {
      /* A      R(A) := target_class */
      if (!mrb->c->ci->target_class) {
        mrb_value exc = mrb_exc_new_str_lit(mrb, E_TYPE_ERROR, "no target class or module");
        mrb_exc_set(mrb, exc);
        goto L_RAISE;
      }
      regs[GETARG_A(i)] = mrb_obj_value(mrb->c->ci->target_class);
      NEXT;
    }

    CASE(OP_RANGE) {
      /* A B C  R(A) := range_new(R(B),R(B+1),C) */
      int b = GETARG_B(i);
      mrb_value val = mrb_range_new(mrb, regs[b], regs[b+1], GETARG_C(i));
      regs[GETARG_A(i)] = val;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_DEBUG) {
      /* A B C    debug print R(A),R(B),R(C) */
#ifdef MRB_ENABLE_DEBUG_HOOK
      mrb->debug_op_hook(mrb, irep, pc, regs);
#else
#ifndef MRB_DISABLE_STDIO
      printf("OP_DEBUG %d %d %d\n", GETARG_A(i), GETARG_B(i), GETARG_C(i));
#else
      abort();
#endif
#endif
      NEXT;
    }

    CASE(OP_STOP) {
      /*        stop VM */
    L_STOP:
      while (mrb->c->ci > mrb->c->cibase) {
        cipop(mrb);
      }
      while (mrb->c->eidx > 0) {
        ecall(mrb);
      }
      ERR_PC_CLR(mrb);
      mrb->jmp = prev_jmp;
      if (mrb->exc) {
        return mrb_obj_value(mrb->exc);
      }
    L_HALT:
      return regs[irep->nlocals];
    }

    CASE(OP_ERR) {
      /* Bx     raise RuntimeError with message Lit(Bx) */
      mrb_value msg = mrb_str_dup(mrb, pool[GETARG_Bx(i)]);
      mrb_value exc;

      if (GETARG_A(i) == 0) {
        exc = mrb_exc_new_str(mrb, E_RUNTIME_ERROR, msg);
      }
      else {
        exc = mrb_exc_new_str(mrb, E_LOCALJUMP_ERROR, msg);
      }
      ERR_PC_SET(mrb, pc);
      mrb_exc_set(mrb, exc);
      goto L_RAISE;
    }
  }
  END_DISPATCH;
#undef regs

  }
  MRB_CATCH(&c_jmp) {
    exc_catched = TRUE;
    goto RETRY_TRY_BLOCK;
  }
  MRB_END_EXC(&c_jmp);

L_DISPATCH:
  gtptr = mrbjit_dispatch(mrb, &status);
  i=BYTECODE_DECODER(*pc);
  CODE_FETCH_HOOK(mrb, irep, pc, mrb->c->stack);
  goto *gtptr;
}

MRB_API mrb_value
mrb_run(mrb_state *mrb, struct RProc *proc, mrb_value self)
{
  if (mrb->c->ci->argc < 0) {
    return mrb_vm_run(mrb, proc, self, 3); /* receiver, args and block) */
  }
  else {
    return mrb_vm_run(mrb, proc, self, mrb->c->ci->argc + 2); /* argc + 2 (receiver and block) */
  }
}

MRB_API mrb_value
mrb_top_run(mrb_state *mrb, struct RProc *proc, mrb_value self, unsigned int stack_keep)
{
  mrb_callinfo *ci;
  mrb_value v;

  if (!mrb->c->cibase) {
    return mrb_vm_run(mrb, proc, self, stack_keep);
  }
  if (mrb->c->ci == mrb->c->cibase) {
    return mrb_vm_run(mrb, proc, self, stack_keep);
  }
  ci = cipush(mrb);
  ci->mid = 0;
  ci->nregs = 1;   /* protect the receiver */
  ci->acc = CI_ACC_SKIP;
  ci->target_class = mrb->object_class;
  v = mrb_vm_run(mrb, proc, self, stack_keep);
  cipop(mrb);

  return v;
}

#if defined(MRB_ENABLE_CXX_EXCEPTION) && defined(__cplusplus)
# if !defined(MRB_ENABLE_CXX_ABI)
} /* end of extern "C" */
# endif
mrb_int mrb_jmpbuf::jmpbuf_id = 0;
# if !defined(MRB_ENABLE_CXX_ABI)
extern "C" {
# endif
#endif

#include "mruby.h"
#include "mruby/jit.h"
#include "mruby/irep.h"

void *
mrbjit_invoke(mrb_value *regs, mrb_code **pc, mrb_state *mrb, 
	      struct mrb_context *c, void * entry, 
	      void *(**prev_entry)())
{
  void *(*tmp)();
  void *rc;
#if defined(__x86_64__)
#elif defined(__i386__) || defined(__CYGWIN__)
  asm volatile("mov %0, %%ecx\n\t"
	       "mov %1, %%ebx\n\t"
	       "mov %2, %%esi\n\t"
	       "mov %3, %%edi\n\t"
	       :
	       : "g"(regs),
		 "g"(pc),
		 "g"(mrb),
		 "g"(c)
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
	       : "=c"(tmp));
#else
#error "only i386 or x86-64 is supported"
#endif

  *prev_entry = tmp;

  return rc;
}

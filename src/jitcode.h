/*
** mruby/jitcode.h - Class for XBYAK
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_JITCOD_H
#define MRUBY_JITCODE_H

#include <xbyak/xbyak.h>

class MRBJitCode: public Xbyak::CodeGenerator {

 public:
  const void *emit_mov() {
    const void *code = getCurr();

    return code;
  }
};

#endif  /* MRUBY_JITCODE_H */

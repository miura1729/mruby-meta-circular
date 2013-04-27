#include "mruby.h"
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


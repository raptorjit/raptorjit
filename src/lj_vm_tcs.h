/*
** Trace call state for bytecode VM.
** Copyright (C) 2019 Max Rottenkolber. See Copyright Notice in luajit.h
*/

#ifndef _LJ_VM_TCS_H
#define _LJ_VM_TCS_H

#include "lj_jit.h"
#include "lj_target_x86.h"

enum {
  TRACE_EXIT,
  TRACE_EXIT_INTERP,
  TRACE_EXIT_INTERP_NOTRACK,
};

/* Trace call state. Must match lj_vm_trace_call_*.asm */
typedef struct {
  TValue *base; /* Copy of BASE to avoid chasing lua_State in asm. */
  void *rsp;
  int handler; /* See enum above. */
  ExitNo exitno;
  ExitState exitstate;
} TraceCallState;

#define TCS_MULTRES(tcs) ((int)(tcs).exitstate.gpr[RID_RET])
#define TCS_PC(tcs) ((BCIns *)(tcs).exitstate.gpr[RID_LPC])
#define TCS_BASE(tcs) ((TValue *)(tcs).exitstate.gpr[RID_BASE])

#endif

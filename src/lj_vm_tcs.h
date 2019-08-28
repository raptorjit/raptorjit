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
  void *rsp; /* Stack pointer at trace entry. */
  int handler; /* Handler for trace exit, see enum above. */
  ExitNo exitno; /* Exit taken. */
  ExitState state; /* Register and stack state on entry/exit. */
} TraceCallState;

#define GPR_RET RID_RET
#define GPR_PC RID_LPC
#define GPR_BASE RID_BASE
#define GPR_DISPATCH RID_DISPATCH

#endif

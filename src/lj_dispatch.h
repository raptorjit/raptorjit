/*
** Instruction dispatch handling.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_DISPATCH_H
#define _LJ_DISPATCH_H

#include "lj_obj.h"
#include "lj_bc.h"
#include "lj_jit.h"


/* Type of hot counter. Must match the code in the assembler VM. */
/* 16 bits are sufficient. Only 0.0015% overhead with maximum slot penalty. */
typedef uint16_t HotCount;

/* Number of hot counter hash table entries (must be a power of two). */
#define HOTCOUNT_SIZE		64
#define HOTCOUNT_PCMASK		((HOTCOUNT_SIZE-1)*sizeof(HotCount))

/* Hotcount decrements. */
#define HOTCOUNT_LOOP		2
#define HOTCOUNT_CALL		1

/* This solves a circular dependency problem -- bump as needed. Sigh. */
#define GG_NUM_ASMFF	57

#define GG_LEN_DDISP	(BC__MAX + GG_NUM_ASMFF)
#define GG_LEN_SDISP	BC_FUNCF
#define GG_LEN_DISP	(GG_LEN_DDISP + GG_LEN_SDISP)

/* Global state, main thread and extra fields are allocated together. */
typedef struct GG_State {
  lua_State L;				/* Main thread. */
  global_State g;			/* Global state. */
  jit_State J;				/* JIT state. */
  HotCount hotcount[HOTCOUNT_SIZE];	/* Hot counters. */
  void *tcs;                            /* Current TraceCallState. */
  ASMFunction dispatch[GG_LEN_DISP];	/* Instruction dispatch tables. */
  BCIns bcff[GG_NUM_ASMFF];		/* Bytecode for ASM fast functions. */
} GG_State;

#define GG_OFS(field)	((int)offsetof(GG_State, field))
#define G2GG(gl)	((GG_State *)((char *)(gl) - GG_OFS(g)))
#define J2GG(j)		((GG_State *)((char *)(j) - GG_OFS(J)))
#define L2GG(L)		(G2GG(G(L)))
#define J2G(J)		(&J2GG(J)->g)
#define G2J(gl)		(&G2GG(gl)->J)
#define L2J(L)		(&L2GG(L)->J)
#define GG_G2J		(GG_OFS(J) - GG_OFS(g))
#define GG_G2DISP	(GG_OFS(dispatch) - GG_OFS(g))
#define GG_DISP2G	(GG_OFS(g) - GG_OFS(dispatch))
#define GG_DISP2J	(GG_OFS(J) - GG_OFS(dispatch))
#define GG_DISP2HOT	(GG_OFS(hotcount) - GG_OFS(dispatch))
#define GG_DISP2STATIC	(GG_LEN_DDISP*(int)sizeof(ASMFunction))

/* Dispatch mode bits. */
#define DISPMODE_CALL	0x01	/* Override call dispatch. */
#define DISPMODE_RET	0x02	/* Override return dispatch. */
#define DISPMODE_INS	0x04	/* Override instruction dispatch. */
#define DISPMODE_JIT	0x10	/* JIT compiler on. */
#define DISPMODE_REC	0x20	/* Recording active. */

#define hotcount_get(gg, pc) \
  (gg)->hotcount[(u32ptr(pc)>>2) & (HOTCOUNT_SIZE-1)]
#define hotcount_set(gg, pc, val) \
  (hotcount_get((gg), (pc)) = (HotCount)(val))

/* Dispatch table management. */
LJ_FUNC void lj_dispatch_init(GG_State *GG);
LJ_FUNC void lj_dispatch_init_hotcount(global_State *g);
LJ_FUNC void lj_dispatch_update(global_State *g);

/* Instruction dispatch callback for hooks or when recording. */
LJ_FUNCA void lj_dispatch_ins(lua_State *L, const BCIns *pc);
LJ_FUNCA ASMFunction lj_dispatch_call(lua_State *L, const BCIns*pc);
LJ_FUNCA void lj_dispatch_stitch(jit_State *J, const BCIns *pc);

#if !defined(_BUILDVM_H)
/* Save/restore errno and GetLastError() around hooks, exits and recording. */
#include <errno.h>
#define ERRNO_SAVE	int olderr = errno;
#define ERRNO_RESTORE	errno = olderr;
#else
#define ERRNO_SAVE
#define ERRNO_RESTORE
#endif

#endif

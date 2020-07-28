/*
** RaptorJIT virtual machine bytecode interpreter.
** Copyright (C) 2018-2019 Luke Gorrie, Max Rottenkolber
** See Copyright Notice in luajit.h
*/

#include <assert.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include <math.h>
#include <byteswap.h>

#include "lj_bc.h"
#include "lj_ccall.h"
#include "lj_dispatch.h"
#include "lj_frame.h"
#include "lj_lib.h"
#include "lj_obj.h"
#include "lj_state.h"
#include "lj_vm.h"
#include "lj_tab.h"
#include "lj_meta.h"
#include "lj_func.h"
#include "lj_buf.h"
#include "lj_trace.h"
#include "lj_err.h"
#include "lj_vm_tcs.h"

#ifdef LUA_VM_DEBUG
/* Count of executed instructions for debugger prosperity. */
static volatile uint64_t insctr;

/* Offset for when to start tracing (as in "logging", this has nothing to do
 * with the tracing JIT whatsoever) bytecode execution when debugging the
 * interpreter.
 *
 * You can read the value of `insctr' at moments of interest from GDB and set
 * this variable accordingly to log the executed bytecodes from that point
 * onward.
 */
static volatile uint64_t insctr_tracefrom = UINT64_MAX;

#define TRACE(name)                                                     \
  if (insctr >= insctr_tracefrom)                                       \
    printf("%-6lu %-6s A=%-3d B=%-3d C=%-3d D=%-5d stackdepth=%-3ld%s\n", \
           insctr, name, A, B, C, D, TOP-BASE,                          \
           (G(L)->dispatchmode & DISPMODE_REC) ? " [rec]" : "")
#define TRACEFF(name)                                           \
  if (insctr >= insctr_tracefrom)                               \
    printf("%-6lu ASMFF  OP=%-3x %-10s\n", insctr, OP, name)
#endif
#ifndef LUA_VM_DEBUG
#define TRACE(name) ((void)0)
#define TRACEFF(name) ((void)0)
#endif

#define neg(n) (-1 - (n))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define min(a,b) ((a)<(b) ? (a) : (b))

/* Forward declarations. */
static inline void vm_call(lua_State *L, TValue *callbase, int _nargs, int ftp);
static inline void vm_callt(lua_State *L, int offset, int _nargs);
static int vm_return(lua_State *L, uint64_t link, int resultofs, int nresults);
static inline void vm_call_cont(lua_State *L, TValue *newbase, int _nargs);
static int fff_fallback(lua_State *L);
static inline void vm_dispatch(lua_State *L);
static inline void vm_hotloop(lua_State *L);
static inline void vm_hotcall(lua_State *L);
static inline void vm_exec_trace(lua_State *L, BCReg traceno);
static inline void vm_savepc(lua_State *L, const BCIns *pc);
static inline int32_t tobit(TValue *num);
/* From lib_base.c: */
void lj_ffh_coroutine_wrap_err(lua_State *L, lua_State *co);

/* Simple debug utility. */
#ifdef LUA_VM_DEBUG
void printstack(lua_State *L)
{
  int i;
  for (i = -2; i < L->top - L->base; i++) {
    TValue *v = L->base + i;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}
void printupvalues(GCfuncL *parent)
{
  int i;
  for (i = 0; i < parent->nupvalues; i++) {
    TValue *v = parent->uvptr[i]->uv.v;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}
void printsource(lua_State *L)
{
  lua_Debug ar;
  lua_getstack(L, 0, &ar);
  lua_getinfo(L, "nSl", &ar);
  printf("%s %s:%d\n", ar.what, ar.source, ar.currentline);
}
#endif

/*
 * Random notes:
 *
 * This interpreter keeps the value of L->top consistent between
 * bytecodes. This is helpful for debugging. The assembler VM only
 * updates L->top when calling external code that might use it.
 */


/* -- Virtual machine registers ------------------------------------------- */

/* The "registers" in the virtual machine are simply some important
 * values that frequently instructions manipulate. We give these
 * values short names so that it is easier to talk about what each
 * instruction does in terms of the registers that it manipulates.
 * 
 * We provide macros to abstract over the way each register is
 * represented. We refer to each register by a NAME whether it is
 * represented by a global variable, a slot in the lua_State struct,
 * etc.
 */

/* Backing variables for certain registers. */
static unsigned int multres;
static unsigned int nargs;
static void *kbase;
static const BCIns *pc;
static lua_State *cont_L;
static TValue *cont_base;
ptrdiff_t cont_ofs;

/* Program counter (PC) register stores the address of the next
 * instruction to run after the current instruction finishes.
 *
 * Most instructions simply increment the program counter. Branch and
 * function call instructions load new values to make a jump.
 */
#define PC pc

/* BASE (base stack slot) is the first stack slot for use by the
 * current stack frame.
 *
 * BASE[N>=0] holds the Nth local value accessible in the current
 * function call. These slots include function arguments, local
 * variables, and temporary values.
 *
 * BASE[-1] encodes the state needed to return to the previous frame.
 * BASE[-2] holds a reference to the currently running function.
 *
 * See lj_frame.h for more details of how call frames are linked.
 */
#define BASE (L->base)

/* TOP (top stack frame) is the last valid stack slot.
 *
 * The values BASE..TOP are the local values within the stack frame
 * that are typically referenced as instruction operands. 
 */
#define TOP (L->top)

/* NARGS (number of arguments) register specifies the number of fixed
 * arguments in a function call.
 *
 * NARGS is set by function calling instructions (e.g. CALL) and then
 * read by function header instructions (e.g. FUNCF).
 */
#define NARGS nargs

/* MULTRES (multiple results) register specifies the number of values
 * provided by a multiple-valued instruction. The multiple values are
 * counted separately from (in addition to) any fixed values.
 *
 * MULTRES is read by multiple value call instructions (e.g.
 * CALLM) and set by multiple value return instructions (e.g. RETM, FUNCC).
 */
/* XXX Rename to e.g. "MULTVAL" since this is used for both results and arguments? */
#define MULTRES multres

/* KBASE (constant base) register specifies the base address of the
 * array of constants that can be referenced by instructions. The
 * constants are specific to each function definition (prototype.) 
 *
 * The array is divided into two parts: pointer constants (GCobj*) at
 * negative indices and tagged-value (TValue) constants at
 * non-negative indecies. Specifically,
 *
 * KBASE[N>=0] holds the Nth TValue constant.
 * KBASE[N<0] holds the ~Nth (bitwise complement of N) GCptr* constant.
 */
#define KBASE kbase

/* STATE describes what kind of code the virtual machine is running.
 *
 * STATE=<0 is a complemented value from the LJ_VMST enum e.g.
 * ~LJ_VMST_INTERP or ~LJ_VMST_GC or ~LJ_VMST_C.
 *
 * STATE>0 is the number of the trace whose machine code is running.
 */
#define STATE (G(L)->vmstate)

/* Registers OP, A, B, C, and D are loaded with the opcode and
 * operands of the current instruction. Exactly which of these
 * registers contains a valid value varies between different
 * instructions.
 *
 * Note: Changing PC does not automatically update these registers.
 */
#define OP bc_op(curins)
#define A  bc_a(curins)
#define B  bc_b(curins)
#define C  bc_c(curins)
#define D  bc_d(curins)

/* Registers CONT_L, CONT_BASE, and CONT_OFS are set when returning from
 * metamethod continuation frames (FRAME_CONT case in vm_return) and used by
 * metamethod continuations (lj_cont_*). CONT_L is the active lua_State and
 * CONT_BASE is the stack base of the continuation.
 *
 * The results (if any) of the continuation start at CONT_BASE+CONT_OFS. The
 * MULTRES register is used to pass the number of available results.
 */
#define CONT_L cont_L
#define CONT_BASE cont_base
#define CONT_OFS cont_ofs


/* -- Utility functions --------------------------------------------------- */

/* Copy values from 'src' to 'dst' and fill missing values with nil.
 * Return pointer to element after the last one filled in dst.
 */
static TValue *copyTVs(lua_State *L, TValue *dst, TValue *src,
                       int need, int have)
{
  int ncopy = min(need, have);
  int npad  = max(0, need - have);
  lua_assert(need>=0);
  lua_assert(have>=0);
  while (ncopy-- > 0) copyTV(L, dst++, src++);
  while (npad--  > 0) setnilV(dst++);
  return dst;
}

/* Return the nth constant TValue. */
static inline TValue* ktv(int n)
{
  return (TValue*)KBASE + n;
}

/* Return the nth constant GC object. */
static inline const GCobj* kgc(int n)
{
  return *((const GCobj**)KBASE-1-n);
}

static inline void branchPC(int offset)
{
  PC += offset - BCBIAS_J;
}

/* Reference the nth constant GC object with known type. */
#define kgcref(n, type) ((type *)kgc(n))


/* Execute virtual machine instructions in a tail-recursive loop. */
void execute(lua_State *L) {
  BCIns curins;
 execute:
  #ifdef LUA_VM_DEBUG
  insctr++;
  #endif
  vm_dispatch(L); /* Load curins after! (Can patch next bytecode at *PC.) */
  curins = *PC++;
  switch (OP) {
  case BC_ISLT:
    /* ISLT: Take following JMP instruction if A < D. */
    if (OP == BC_ISLT) TRACE("ISLT");
  case BC_ISGE:
    /* ISGE: Take following JMP instruction if A >= D. */
    if (OP == BC_ISGE) TRACE("ISGE");
  case BC_ISLE:
    /* ISLE: Take following JMP instruction if A <= D. */
    if (OP == BC_ISLE) TRACE("ISLE");
  case BC_ISGT:
    /* ISGT: Take following JMP instruction if A > D. */
    if (OP == BC_ISGT) TRACE("ISGT");
    {
      int flag = 0;
      if (tvisnum(BASE+A) && tvisnum(BASE+D)) {
        double x = BASE[A].n, y = BASE[D].n;
        /* Compare two floats.
         *
         * Note: to preserve NaN semantics GE/GT branch on unordered, but LT/LE
         * don't.
         */
        if (OP == BC_ISLT)
          flag = x < y;
        else if (OP == BC_ISGE)
          flag = x >= y || isnan(x) || isnan(y);
        else if (OP == BC_ISLE)
          flag = x <= y;
        else if (OP == BC_ISGT)
          flag = x > y || isnan(x) || isnan(y);
      } else {
        /* Fall back to meta-comparison. */
        vm_savepc(L, PC);
        TValue *res = lj_meta_comp(L, BASE+A, BASE+D, OP);
        if ((intptr_t)res > 1) {
          vm_call_cont(L, res, 2);
          break; /* Do not clobber PC! */
        } else
          flag = (intptr_t)res == 1;
      }
      /* Advance to jump instruction. */
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQV:
    /* ISEQV: Take following JMP instruction if A is equal to D. */
    TRACE("ISEQV");
  case BC_ISNEV:
    /* ISNEV: Take following JMP instruction if A is not equal to D. */
    if (OP == BC_ISNEV)
      TRACE("ISNEV");
    {
      TValue *x = BASE+A; TValue *y = BASE+D;
      int flag = (OP == BC_ISNEV); // Invert flag on ISNEV.
      if (tvisnum(x) && tvisnum(y))
        flag ^= (x->n == y->n);
      else if (tviscdata(x) || tviscdata(y)) {
        // Either object is cdata.
        vm_savepc(L, (BCIns*)((intptr_t)PC-4));
        TValue *res = lj_meta_equal_cd(L, curins);
        if ((intptr_t)res > 1) {
          vm_call_cont(L, res, 2);
          break; /* Do not clobber PC! */
        } else
          flag = (intptr_t)res;
      } else if (x->u64 == y->u64)
        // Same GCobjs or pvalues?
        flag ^= 1;
      else if (itype(x) != itype(y))
        // Not the same type?
        flag = flag;
      else if (itype(x) <= LJ_TISTABUD) {
        // Different tables or userdatas. Need to check __eq metamethod.
        vm_savepc(L, (BCIns*)((intptr_t)PC-4));
        TValue *res = lj_meta_equal(L, gcval(BASE+A), gcval(BASE+D), flag);
        if ((intptr_t)res != flag) {
          vm_call_cont(L, res, 2);
          break; /* Do not clobber PC! */
        }
      }
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQS:
    /* ISEQS: Take following JMP instruction if A is equal to string D. */
    TRACE("ISEQS");
  case BC_ISNES:
    /* ISNES: Take following JMP instruction if A is not equal to string D. */
    if (OP == BC_ISNES)
      TRACE("ISNES");
    {
      int flag = (OP == BC_ISNES); // Invert flag on ISNES.
      if (tvisstr(BASE+A))
        flag ^= (strV(BASE+A) == kgcref(D, GCstr));
      else if (tviscdata(BASE+A))
        assert(0 && "NYI: ISEQS/ISNES on cdata.");
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQN:
    /* ISEQN: Take following JMP if A is equal to number constant D. */
    TRACE("ISEQN");
  case BC_ISNEN:
    /* ISNEN: Take following JMP if A is not equal to number constant D. */
    if (OP == BC_ISNEN)
      TRACE("ISNEN");
    {
      int flag = (OP == BC_ISNEN); // Invert flag on ISNEN.
      if (tvisnum(BASE+A))
        flag ^= numV(BASE+A) == numV(ktv(D));
      else if (tviscdata(BASE+A)) {
        vm_savepc(L, (BCIns*)((intptr_t)PC-4));
        TValue *res = lj_meta_equal_cd(L, curins);
        if ((intptr_t)res > 1) {
          vm_call_cont(L, res, 2);
          break; /* Do not clobber PC! */
        } else
          flag = (intptr_t)res;
      }
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQP:
    /* ISEQP: Take following JMP if A is equal to primtive D.*/
    TRACE("ISEQP");
  case BC_ISNEP:
    /* ISNEP: Take following JMP unless A is equal to primtive D.*/
    if (OP == BC_ISNEP)
      TRACE("ISNEP");
    {
      int flag = (OP == BC_ISNEP); // Invert flag on ISNEP.
      if (itype(BASE+A) == ~D)
        /* If the types match than A is nil/false/true and equal to pri D. */
        flag ^= 1;
      else if (tviscdata(BASE+A))
        assert(0 && "NYI: ISEQP/ISNEP on cdata.");
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISTC:
    /* ISTC: Copy D to A and take following JMP instruction if D is true. */
    TRACE("ISTC");
  case BC_ISFC:
    /* ISFC: Copy D to A and take following JMP instruction if D is false. */
    if (OP == BC_ISFC)
      TRACE("ISFC");
    {
      int flag = (OP == BC_ISFC); // Invert flag on ISFC.
      BASE[A] = BASE[D];
      flag ^= tvistruecond(BASE+D);
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_IST:
    /* IST: Take following JMP instruction if D is true. */
    TRACE("IST");
    {
      int flag = tvistruecond(BASE+D);
      /* Advance to jump instruction. */
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISF:
    TRACE("ISF");
    {
      int flag = !tvistruecond(BASE+D);
      /* Advance to jump instruction. */
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISTYPE:
    /* ISTYPE: assert A is of type -D. */
    TRACE("ISTYPE");
    if (itype(BASE+A) != -D) {
      vm_savepc(L, PC);
      lj_meta_istype(L, A, D);
    }
    break;
  case BC_ISNUM:
    /* ISNUM: assert A is a number. */
    TRACE("ISNUM");
    if (!tvisnum(BASE+A)) {
      vm_savepc(L, PC);
      lj_meta_istype(L, A, D);
    }
    break;
  case BC_MOV:
    TRACE("MOV");
    /* MOV: A = dst; D = src */
    copyTV(L, BASE+A, BASE+D);
    break;
  case BC_NOT:
    /* NOT: Set A to boolean not of D. */
    TRACE("NOT");
    setboolV(BASE+A, !tvistruecond(BASE+D));
    break;
  case BC_UNM:
    /* UNM: Set A to -D (unary minus). */
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+D, BASE+D, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_LEN:
    /* LEN: Set A to #D (object length). */
    TRACE("LEN");
    {
      TValue *dst = BASE+A;
      TValue *o = BASE+D;
      if (tvisstr(o))
        setnumV(dst, strV(o)->len);
      else if (tvistab(o)) {
        /* Lua 5.1 does not support __len on tables. */
        setnumV(dst, lj_tab_len(tabV(o)));
      } else {
        vm_savepc(L, PC);
        vm_call_cont(L, lj_meta_len(L, o), 1);
      }
    }
    break;
  case BC_ADDVN:
    /* ADDVN: Add number constant C to B and store the result in A. */
    TRACE("ADDVN");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, ktv(C), OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_SUBVN:
    /* SUBVN: Subtract number constant C from B and store the result in A. */
    TRACE("SUBVN");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, ktv(C), OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_MULVN:
    /* MULVN: Multiply B by number constant C and store the result in A. */
    TRACE("MULVN");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, ktv(C), OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_DIVVN:
    /* DIVVN: Divide B by number constant C and store the result in A. */
    TRACE("DIVVN");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, ktv(C), OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_MODVN:
    /* MODVN: Calculate B modulo number constant C and store the result in A. */
    TRACE("MODVN");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, ktv(C), OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_ADDNV:
    /* ADDNV: Add B to number constant C and store the result in A. */
    TRACE("ADDNV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, ktv(C), BASE+B, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_SUBNV:
    /* SUBNV: Subtract B from number constant C and store the result in A. */
    TRACE("SUBNV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, ktv(C), BASE+B, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_MULNV:
    /* MULNV: Multiply number constant C by B and store the result in A. */
    TRACE("MULNV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, ktv(C), BASE+B, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_DIVNV:
    /* DIVNV: Divide number constant C by B and store the result in A. */
    TRACE("DIVNV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, ktv(C), BASE+B, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_MODNV:
    /* MODNV: Calculate number constant C modulo B and store the result in A. */
    TRACE("MODNV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, ktv(C), BASE+B, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_ADDVV:
    /* ADDVV: Add C to B and store the result in A. */
    TRACE("ADDVV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_SUBVV:
    /* SUBVV: Subtract C from B and store the result in A. */
    TRACE("SUBVV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_MULVV:
    /* MULVV: Multiply B by C and store the result in A. */
    TRACE("MULVV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_DIVVV:
    /* DIVVV: Divide B by C and store the result in A. */
    TRACE("DIVVV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_MODVV:
    /* MODVV: Calculate B modulo C and store the result in A. */
    TRACE("MODVV");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_POW:
    /* POW: Calculate power C of B and store the result in A. */
    TRACE("POW");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
      if (mbase) vm_call_cont(L, mbase, 2);
    }
    break;
  case BC_CAT:
    TRACE("CAT");
    vm_savepc(L, PC);
    {
      TValue *mbase = lj_meta_cat(L, BASE+C, C-B);
      if (mbase) vm_call_cont(L, mbase, 2);
      else copyTV(L, BASE+A, BASE+B);
    }
    break;
  case BC_KSTR:
    TRACE("KSTR");
    setgcVraw(BASE+A, kgcref(D, GCobj), LJ_TSTR);
    break;
  case BC_KCDATA:
    /* KCDATA: Set A to cdata constant D. */
    TRACE("KCDATA");
    setcdataV(L, BASE+A, kgcref(D, GCcdata));
    break;
  case BC_KSHORT:
    TRACE("KSHORT");
    /* BASE[A] = D */
    setnumV(BASE+A, (int16_t) D); // D is a signed int16 literal.
    break;
  case BC_KNUM:
    /* KNUM: Set slot A to number constant D. */
    TRACE("KNUM");
    setnumV(BASE+A, ktv(D)->n);
    break;
  case BC_KPRI:
    TRACE("KPRI");
    setpriV(BASE+A, ~D); // D is 0/1/2 for nil/false/true.
    break;
  case BC_KNIL:
    /* KNIL: Set slots A to D to nil. */
    TRACE("KNIL");
    copyTVs(L, BASE+A, NULL, 1+D - A, 0);
    break;
  case BC_UGET:
    TRACE("UGET");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      BASE[A] = *mref(parent->uvptr[D]->uv.v, TValue);
    }
    break;
  case BC_USETV:
    /* USETV: Set upvalue A to D. */
    TRACE("USETV");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCupval *uv = &parent->uvptr[A]->uv;
      TValue *v = (TValue *)uv->v;
      copyTV(L, v, BASE+D);
      // Upvalue closed, marked black, and new value is collectable and white?
      if (uv->closed && (uv->marked & LJ_GC_BLACK)
          && tvisgcv(v) && iswhite(gcval(v)))
        // Crossed a write barrier. Move the barrier forward.
        lj_gc_barrieruv(G(L), v);
    }
    break;
  case BC_USETS:
    /* USETS: Set upvalue A to string constant D. */
    TRACE("USETS");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCupval *uv = &parent->uvptr[A]->uv;
      TValue *v = (TValue *)uv->v;
      GCobj *o = kgcref(D, GCobj);
      setgcVraw(v, o, LJ_TSTR);
      // Upvalue closed, marked black, and new value is white?
      if (uv->closed && (uv->marked & LJ_GC_BLACK) && iswhite(o))
        // Crossed a write barrier. Move the barrier forward.
        lj_gc_barrieruv(G(L), v);
    }
    break;
  case BC_USETN:
    /* USETN: Set upvalue A to number constant D. */
    TRACE("USETN");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCupval *uv = &parent->uvptr[A]->uv;
      TValue *v = (TValue *)uv->v;
      setnumV(v, numV(ktv(D)));
    }
    break;
  case BC_USETP:
    /* USETP: Set upvalue A to primitive D. */
    TRACE("USETP");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCupval *uv = &parent->uvptr[A]->uv;
      TValue *v = (TValue *)uv->v;
      setpriV(v, ~D);
    }
    break;
  case BC_UCLO:
    /* UCLO: Close upvalues for slots ≥ rbase and jump to target D. */
    TRACE("UCLO");
    if (L->openupval > (GCRef)0)
      lj_func_closeuv(L, BASE+A);
    branchPC(D);
    break;
  case BC_FNEW:
    TRACE("FNEW");
    vm_savepc(L, PC);
    {
      GCproto *pt = kgcref(D, GCproto);
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCfunc *fn = lj_func_newL_gc(L, pt, parent);
      setgcVraw(BASE+A, (GCobj*)fn, LJ_TFUNC);
    }
    break;
  case BC_TNEW:
    TRACE("TNEW");
    vm_savepc(L, PC);
    lj_gc_check(L);
    {
      uint32_t asize = D & ((1<<11)-1);
      uint32_t hbits = D >> 11;
      GCtab *tab = lj_tab_new(L, asize, hbits);
      setgcVraw(BASE+A, (GCobj*)tab, LJ_TTAB);
    }
    break;
  case BC_TDUP:
    TRACE("TDUP");
    vm_savepc(L, PC);
    lj_gc_check(L);
    {
      GCtab *tab = lj_tab_dup(L, kgcref(D, GCtab));
      setgcVraw(BASE+A, (GCobj*)tab, LJ_TTAB);
    }
    break;
  case BC_GGET:
    TRACE("GGET");
    /* A = _G[D] */
    vm_savepc(L, PC);
    {
      GCfunc *fn = funcV(BASE-2);
      TValue e, k;
      const TValue *v;
      setgcVraw(&e, fn->l.env, LJ_TTAB);
      setgcVraw(&k, kgcref(D, GCobj), LJ_TSTR);
      v = lj_meta_tget(L, &e, &k);
      if (v)
        copyTV(L, BASE+A, v);
      else
        vm_call_cont(L, TOP, 2);
    }
    break;
  case BC_GSET:
    /* _G[D] = A */
    TRACE("GSET");
    vm_savepc(L, PC);
    {
      GCfunc *fn = funcV(BASE-2);
      TValue e, k, *v;
      setgcVraw(&e, fn->l.env, LJ_TTAB);
      setgcVraw(&k, kgcref(C, GCobj), LJ_TSTR);
      v = lj_meta_tset(L, &e, &k);
      if (v) {
        copyTV(L, v, BASE+A);
      } else {
        copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
        vm_call_cont(L, TOP, 3);
      }
    }
    break;
  case BC_TGETV:
    /* TGETV: A = B[C] */
    TRACE("TGETV");
    vm_savepc(L, PC);
    {
      const TValue *v = lj_meta_tget(L, BASE+B, BASE+C);
      if (v)
        copyTV(L, BASE+A, v);
      else
        vm_call_cont(L, TOP, 2);
    }
    break;
  case BC_TGETS:
    /* TGETS: A = B[C] where C is a string constant. */
    TRACE("TGETS");
    vm_savepc(L, PC);
    {
      TValue k;
      const TValue *v;
      setgcVraw(&k, kgcref(C, GCobj), LJ_TSTR);
      v = lj_meta_tget(L, BASE+B, &k);
      if (v)
        copyTV(L, BASE+A, v);
      else
        vm_call_cont(L, TOP, 2);
    }
    break;
  case BC_TGETB:
    /* TGETB: A = B[C] where C is a byte literal. */
    TRACE("TGETB");
    vm_savepc(L, PC);
    {
      TValue k;
      const TValue *v;
      k.n = C;
      v = lj_meta_tget(L, BASE+B, &k);
      if (v)
        copyTV(L, BASE+A, v);
      else
        vm_call_cont(L, TOP, 2);
    }
    break;
  case BC_TGETR:
    /* TGETR: A = B[C]  (__index is ignored). */
    TRACE("TGETR");
    copyTV(L, BASE+A, lj_tab_getint(tabV(BASE+B), (int32_t)numV(BASE+C)));
    break;
  case BC_TSETV:
    TRACE("TSETV");
    vm_savepc(L, PC);
    {
      TValue *v = lj_meta_tset(L, BASE+B, BASE+C);
      if (v) {
        copyTV(L, v, BASE+A);
      } else {
        copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
        vm_call_cont(L, TOP, 3);
      }
    }
    break;
  case BC_TSETS:
    TRACE("TSETS");
    vm_savepc(L, PC);
    {
      TValue k, *v;
      setgcVraw(&k, kgcref(C, GCobj), LJ_TSTR);
      v = lj_meta_tset(L, BASE+B, &k);
      if (v) {
        copyTV(L, v, BASE+A);
      } else {
        copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
        vm_call_cont(L, TOP, 3);
      }
      break;
    }
  case BC_TSETB:
    /* TSETB: B[C] = A where C is an unsigned literal. */
    TRACE("TSETB");
    vm_savepc(L, PC);
    {
      TValue k, *v;
      k.n = C;
      v = lj_meta_tset(L, BASE+B, &k);
      if (v) {
        copyTV(L, v, BASE+A);
      } else {
        copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
        vm_call_cont(L, TOP, 3);
      }
    }
    break;
  case BC_TSETM:
    TRACE("TSETM");
    vm_savepc(L, PC);
    {
      unsigned int i = 0, ix = ktv(D)->u32.lo;
      TValue *o = BASE+A-1;
      GCtab *tab = tabV(o);
      if (tab->asize < ix+MULTRES)
        lj_tab_reasize(L, tab, ix + MULTRES);
      for (i = 0; i < MULTRES; i++)
        *arrayslot(tab, ix+i) = BASE[A+i];
      lj_gc_anybarriert(L, tab);
    }
    break;
  case BC_TSETR:
    /* TGETR: B[C] = A (__newindex is ignored.) */
    TRACE("TSETR");
    vm_savepc(L, PC);
    copyTV(L, lj_tab_setint(L, tabV(BASE+B), (int32_t)numV(BASE+C)), BASE+A);
    break;
  case BC_CALLM: case BC_CALL:
    if (OP == BC_CALLM) {
      /* CALLM: A = newbase, B = nresults+1, C = extra_nargs */
      TRACE("CALLM");
      NARGS = C+MULTRES; /* nargs in MULTRES */
    } else if (OP == BC_CALL) {
      /* CALL: A = newbase, B = nresults+1, C = nargs+1 */
      TRACE("CALL");
      NARGS = C-1;
    }
    {
      /* Notes:
       *
       * PC is 32-bit aligned and so the low bits are always 00 which
       * corresponds to the FRAME_LUA tag value.
       *
       * CALL does not have to record the number of expected results
       * in the frame data. The callee's RET bytecode will locate this
       * CALL and read the value from the B operand. */
      vm_call(L, BASE+2+A, NARGS, FRAME_LUA);
    }
    break;
  case BC_CALLMT: case BC_CALLT:
    if (OP == BC_CALLMT) {
      /* CALLMT: Tailcall A(A+1, ..., A+D+MULTRES) */
      TRACE("CALLMT");
      NARGS = D+MULTRES; /* nargs in MULTRES */
    } else if (OP == BC_CALLT) {
      /* CALLT: Tailcall A(A+1, ..., A+D-1). */
      TRACE("CALLT");
      NARGS = D-1;
    }
    {
      MULTRES = NARGS;
      vm_callt(L, A, NARGS);
    }
    break;
  case BC_ITERC:
    TRACE("ITERC");
    {
      TValue *fb = BASE+A+2;
      fb[0] = fb[-4]; // Copy state.
      fb[1] = fb[-3]; // Copy control var.
      fb[-2] = fb[-5]; // Copy callable.
      vm_call(L, fb, 2, FRAME_LUA);
    }
    break;
  case BC_ITERN:
    /* ITERN: Specialized ITERC, if iterator function A-3 is next(). */
    TRACE("ITERN");
    {
      // NYI: add hotloop, record BC_ITERN.
      GCtab *tab = tabV(BASE + A-2);
      TValue *state = BASE + A-1;
      TValue *key = BASE + A+0;
      TValue *val = BASE + A+1;
      unsigned int i = state->i;
      /* Advance to ITERL instruction. */
      curins = *PC++;
      /* Traverse array part. */
      while (i < tab->asize) {
        cTValue *entry = arrayslot(tab, i);
        if (tvisnil(entry) && ++i) continue; // Skip holes in array part.
        /* Return array index as a numeric key. */
        setnumV(key, i);
        /* Copy array slot to returned value. */
        *val = *entry;
        /* Update control var. */
        state->i = i+1;
        goto itern_next;
      }
      /* Traverse hash part. */
      i -= tab->asize;
      while (i <= tab->hmask) {
        Node *n = &noderef(tab->node)[i];
        if (tvisnil(&n->val) && ++i) continue; // Skip holes in hash part.
        /* Copy key and value from hash slot. */
        *key = n->key;
        *val = n->val;
        /* Update control var. */
        state->i = tab->asize + i+1;
        goto itern_next;
      }
      goto itern_end;
    itern_next:
      /* Iterate: branch to target from ITERL. */
      branchPC(D);
    itern_end:
      /* End of iteration: advance to ITERL+1. */
      break;
    }
  case BC_VARG:
    TRACE("VARG");
    {
      int delta = BASE[-1].u64 >> 3;
      MULTRES = max(delta-2-(int)C, 0);
      copyTVs(L, BASE+A, BASE-delta+C, B>0 ? B-1 : MULTRES, MULTRES);
    }
    break;
  case BC_ISNEXT:
    /* ISNEXT: Verify ITERN specialization and jump. */
    TRACE("ISNEXT");
    {
      TValue *fn = BASE + A-3;
      TValue *tab = BASE + A-2;
      TValue *nil = BASE + A-1;
      branchPC(D);
      if (tvisfunc(fn)
          && funcV(fn)->c.ffid == FF_next_N
          && tvistab(tab)
          && tvisnil(nil))
        BASE[A-1].u64 = U64x(fffe7fff, 00000000); // Initialize control var.
      else
        /* Despecialize bytecode if any of the checks fail. */
        setbc_op(PC, BC_ITERC);
    }
    break;
  case BC_RETM:
    TRACE("RETM");
    if (vm_return(L, BASE[-1].u64, A, D+MULTRES)) return;
    break;
  case BC_RET:
    TRACE("RET");
    if (vm_return(L, BASE[-1].u64, A, D-1)) return;
    break;
  case BC_RET0:
    TRACE("RET0");
    if (vm_return(L, BASE[-1].u64, A, 0)) return;
    break;
  case BC_RET1:
    TRACE("RET1");
    if (vm_return(L, BASE[-1].u64, A, 1)) return;
    break;
  case BC_FORL:
    TRACE("FORL");
    vm_hotloop(L);
  case BC_JFORL:
    if (OP == BC_JFORL) TRACE("JFORL");
  case BC_IFORL:
    if (OP == BC_IFORL) TRACE("IFORL");
    {
      TValue *state = BASE + A;
      TValue *idx = state, *stop = state+1, *step = state+2, *ext = state+3;
      if (OP == BC_JFORL) {
        assert(tvisnum(stop));
        assert(tvisnum(step));
      } else if (!tvisnum(idx) || !tvisnum(stop) || !tvisnum(step))
        lj_meta_for(L, state);
      /* Update loop index. */
      setnumV(idx, idx->n + step->n);
      /* Copy loop index to stack. */
      setnumV(ext, idx->n);
      /* Check for termination */
      if ((step->n >= 0 && idx->n <= stop->n) ||
          (step->n <  0 && stop->n <= idx->n)) {
        if (OP == BC_JFORL)
          vm_exec_trace(L, D);
        else
          branchPC(D);
      }
    }
    break;
  case BC_JFORI:
    TRACE("JFORI");
  case BC_FORI:
    if (OP == BC_FORI) TRACE("FORI");
    vm_savepc(L, PC);
    {
      TValue *state = BASE + A;
      TValue *idx = state, *stop = state+1, *step = state+2, *ext = state+3;
      /* Initialize loop parameters. */
      lj_meta_for(L, state);
      /* Copy loop index to stack. */
      setnumV(ext, idx->n);
      /* Check for termination */
      if ((step->n >= 0 && idx->n > stop->n) ||
          (step->n <  0 && stop->n > idx->n)) {
        branchPC(D);
      } else if (OP == BC_JFORI) {
        /* Always branch in JFORI. */
        branchPC(D);
        /* Continue with trace in found in JFORL bytecode. */
        vm_exec_trace(L, bc_d(*(PC-1)));
      }
    }
    break;
  case BC_ITERL:
    TRACE("ITERL");
    vm_hotloop(L);
  case BC_IITERL:
    if (OP == BC_IITERL) TRACE("IITERL");
    if (!tvisnil(BASE+A)) {
      /* Save control var and branch. */
      branchPC(D);
      BASE[A-1] = *(BASE+A);
    }
    break;
  case BC_JITERL:
    TRACE("JITERL");
    if (!tvisnil(BASE+A)) {
      BASE[A-1] = *(BASE+A);
      vm_exec_trace(L, D);
    }
    break;
  case BC_LOOP:
    TRACE("LOOP");
    vm_hotloop(L);
  case BC_ILOOP:
    if (OP == BC_ILOOP) TRACE("ILOOP");
    break;
  case BC_JLOOP:
    TRACE("JLOOP");
    vm_exec_trace(L, D);
    break;
  case BC_JMP:
    TRACE("JMP");
    branchPC(D);
    break;
  case BC_FUNCF:
    TRACE("FUNCF");
    vm_hotcall(L);
  case BC_IFUNCF:
    if (OP == BC_IFUNCF) TRACE("IFUNCF");
  case BC_JFUNCF:
    if (OP == BC_JFUNCF) TRACE("JFUNCF");
    {
      GCproto *pt = (GCproto*)((intptr_t)(PC-1) - sizeof(GCproto));
      TOP = BASE + A;
      KBASE = mref(pt->k, void);
      assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
      /* Fill missing args with nil. */
      if (A > NARGS) copyTVs(L, BASE+NARGS, NULL, A-NARGS, 0);
    }
    if (OP == BC_JFUNCF)
      vm_exec_trace(L, D);
    break;
  case BC_FUNCV:
    TRACE("FUNCV");
    {
      GCproto *pt = (GCproto*)((intptr_t)(PC-1) - sizeof(GCproto));
      TOP = BASE + A;
      KBASE = mref(pt->k, void);
      assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
      /* Save base of frame containing all parameters. */
      TValue *oldbase = BASE;
      /* Base for new frame containing only fixed parameters. */
      BASE += 2 + NARGS;
      copyTV(L, BASE-2, oldbase-2);
      BASE[-1].u64 = FRAME_VARG + ((BASE - oldbase) << 3);
      copyTVs(L, BASE, oldbase, pt->numparams, NARGS);
      /* Fill moved args with nil. */
      copyTVs(L, oldbase, NULL, min(pt->numparams, NARGS), 0);
    }      
    break;
  case BC_IFUNCV: assert(0 && "NYI BYTECODE: IFUNCV");
  case BC_JFUNCV: assert(0 && "NYI BYTECODE: JFUNCV");
  case BC_FUNCCW: assert(0 && "NYI BYTECODE: FUNCCW");
  case BC_FUNCC:
    TRACE("FUNCC");
    /* 
    ** Call C function.
    */
    {
      int nresults, resultofs;
      lua_CFunction *f = &funcV(BASE-2)->c.f; /* C function pointer */
      TOP = BASE + NARGS;
      assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
      STATE = ~LJ_VMST_C;
      nresults = (*f)(L);
      STATE = ~LJ_VMST_INTERP;
      resultofs = TOP - (BASE + nresults);
      if (vm_return(L, BASE[-1].u64, resultofs, nresults)) return;
    }
    break;
  default:
    /*
      XXX - handle ASM fast functions.
      FIXME: need symbols for pseudo opcodes.
    */
    switch ((uint32_t)OP) {
    case 0x61:
      TRACEFF("assert");
      if (NARGS >= 1 && tvistruecond(BASE)) {
        if (vm_return(L, BASE[-1].u64, 0, NARGS)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x62:
      TRACEFF("type");
      if (NARGS >= 1) {
        uint32_t type = itype(BASE);
        GCfuncC *f = &funcV(BASE-2)->c;
        if (type < LJ_TISNUM)
          type = LJ_TISNUM;
        type = ~type;
        BASE[-2] = f->upvalue[type];
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x63:
      TRACEFF("next");
      if (NARGS >= 1 && tvistab(BASE)) {
        if (NARGS < 2) setnilV(BASE+1);
        TOP = BASE;
        vm_savepc(L, (BCIns*)BASE[-1].u64);
        if (lj_tab_next(L, tabV(BASE), BASE+1)) {
          /* Copy key and value to results. */
          if (vm_return(L, BASE[-1].u64, 1, 2)) return;
        } else {
          /* End of traversal: return nil. */
          setnilV(BASE-2);
          if (vm_return(L, BASE[-1].u64, -2, 1)) return;
        }
      } else if (fff_fallback(L)) return;
      break;
    case 0x64:
      TRACEFF("pairs");
      /* XXX - punt to fallback. */
      if (fff_fallback(L)) return;
      break;
    case 0x65:
      TRACEFF("ipairs_aux");
      if (NARGS >= 2 && tvistab(BASE) && tvisnum(BASE+1)) {
        TValue *tab = BASE;
        TValue *i = BASE+1;
        const TValue *v;
        uint64_t link = BASE[-1].u64;
        /* Increment index. */
        uint32_t n = numV(i) + 1;
        setnumV(BASE-2, n);
        /* Try to load value from table (if this fails the iterator ends.)  */
        if (n < tabV(tab)->asize) {
          /* Value is in array part of tab. */
          v = arrayslot(tabV(tab), n);
          if (tvisnil(v)) goto ipairs_end;
        } else {
          if (!tabV(tab)->hmask) goto ipairs_end;
          v = lj_tab_getinth(tabV(tab), n);
          if (!v) goto ipairs_end;
        }
        BASE[-1] = *v; /* Copy array slot. */
        vm_return(L, link, -2, 2); /* Iterate: return (i, value). */
        break;
        /* End of interator: return no values. */
      ipairs_end:
        if (vm_return(L, link, -2, 0)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x66:
      TRACEFF("ipairs");
      /* XXX - punt to fallback. */
      if (fff_fallback(L)) return;
      break;
    case 0x67:
      TRACEFF("getmetatable");
      if (NARGS > 0) {
        GCtab *mt;
        if (tvistab(BASE))
          mt = tabref(tabV(BASE)->metatable);
        else if (tvisudata(BASE))
          mt = tabref(udataV(BASE)->metatable);
        else
          mt = tabref(basemt_obj(G(L), BASE));
        if (mt) {
          cTValue *mo = lj_tab_getstr(mt, mmname_str(G(L), MM_metatable));
          if (mo)
            copyTV(L, BASE-2, mo);
          else
            setgcVraw(BASE-2, (GCobj*)mt, LJ_TTAB);
        } else {
          setnilV(BASE-2);
        }
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x68:
      TRACEFF("setmetatable");
      /* XXX - punt to fallback. */
      if (fff_fallback(L)) return;
      break;
    case 0x6a:
      TRACEFF("tonumber");
      if (NARGS == 1 && tvisnumber(BASE)) {
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x69:
      TRACEFF("rawget");
      if (NARGS >= 2 && tvistab(BASE)) {
        copyTV(L, BASE, lj_tab_get(L, tabV(BASE), BASE+1));
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x6b:
      TRACEFF("tostring");
      /* XXX - punt to fallback. */
      if (fff_fallback(L)) return;
      break;
    case 0x6c:
      TRACEFF("pcall");
      if (NARGS >= 1) {
        /* First argument (BASE) is the function to call. */
        TValue *callbase = BASE+2;
        int hookflag = hook_active(G(L)) ? 1 : 0;
        int copyargs = NARGS--;
        /* Copy remaining function arguments (from top to avoid clobberin'). */
        while (copyargs--)
          copyTV(L, callbase+copyargs, BASE+1+copyargs);
        vm_call(L, callbase, NARGS, FRAME_PCALL + hookflag);
      } else if (fff_fallback(L)) return;
      break;
    case 0x6d:
      TRACEFF("xpcall");
      if (NARGS >= 2 && tvisfunc(BASE+1)) {
        /* First argument (BASE) is the function to call, second argument
           (BASE+1) is the handler. */
        TValue f;
        TValue *callbase = BASE+3;
        int hookflag = hook_active(G(L)) ? 1 : 0;
        int copyargs = (NARGS -= 2);
        /* Swap function and handler. */
        f = BASE[0]; BASE[0] = BASE[1]; BASE[1] = f;
        /* Copy remaining function arguments (from top to avoid clobberin'). */
        while (copyargs--)
          copyTV(L, callbase+copyargs, BASE+2+copyargs);
        vm_call(L, callbase, NARGS, FRAME_PCALL + hookflag);
      } else if (fff_fallback(L)) return;
      break;
    /*
     * -- Resuming and yielding from coroutines ---------------------------
     *
     * Coroutines (from an implementation standpoint) are implemented as
     * separate execution contexts (lua_State), notably with dedicated
     * stacks, that execute a given function interleaved with the execution
     * of the parent lua_State.
     *
     * An instance of an execution state (represented as a lua_State) is
     * also called a "thread". A thread combined with an "initial function"
     * forms what we call a coroutine.
     *
     * Resuming a coroutine for the first time is like calling the initial
     * function inside the coroutine's thread with the arguments provided
     * (which have to be copied to the coroutine’s stack.)
     *
     * A resumed coroutine can yield result values, or throw an error. A
     * coroutine that has yielded can be resumed again to continue
     * execution from the point where it yielded.
     *
     * Resuming a coroutine again after a yield is like returning to the
     * caller of yield in the coroutine’s thread with the arguments to
     * resume being the result values.
     */
    case 0x6e:
      TRACEFF("coroutine.yield");
      {
        /* Yielding from a coroutine means unlinking its CFrame and setting its
         * thread status to LUA_YIELD before returning to lj_vm_resume.
         */
        if ((intptr_t)L->cframe & CFRAME_RESUME) {
          TOP = BASE+NARGS;
          L->cframe = 0;
          L->status = LUA_YIELD;
          return;
        } else
          if (fff_fallback(L)) return;
      }
      break;
    case 0x6f:
      TRACEFF("coroutine.resume");
    case 0x70:
      if (OP == 0x70)
        TRACEFF("coroutine.wrap_aux");
      {
        /* The code for the resume and the wrap_aux fast functions are similar
         * enough to share most of their code. They differ as follows:
         *
         *   - resume behaves like pcall, catching errors during coroutine
         *     execution and prepending a boolean status value to the results
         *
         *   - wrap_aux behaves like a regular function call, it returns the
         *     results as is, but throws an error if the coroutine fails
         *
         * The code below prepares the stack frame of coroutine `co'. The frame
         * starts at `rbase' and ends at `rtop'. See lj_vm_resume for how the
         * call/return behavior of resume is implemented.
         *
         * On yield resume will return `true' followed by the coroutine’s
         * results. On error resume returns `false' and the error message
         * produced by the coroutine.
         */
        lua_State *co;
        TValue *rbase, *rtop;
        if (OP == 0x6f) {
          /* resume: first argument must be a thread (the coroutine object). */
          if (NARGS >= 1 && tvisthread(BASE))
            co = threadV(BASE);
          else
            goto resume_fallback;
        } else {
          /* wrap_aux: get thread from caller upvalue. */
          co = threadV(&funcV(BASE-2)->c.upvalue[0]);
        }
        /* The thread must not have a CFrame attached. */
        if (co->cframe)
          goto resume_fallback;
        /* The thread's status must be either LUA_OK or LUA_YIELD. */
        if (co->status > LUA_YIELD)
          goto resume_fallback;
        /* If the coroutine is resumed for the first time then we expect the
           initial function at the top of its stack. */
        if (co->status == LUA_OK && co->base == co->top)
          goto resume_fallback;
        /* Prepare frame at coroutine’s TOP. (When resumed for the first time,
           make room for frame link.) */
        rbase = co->top + (co->status == LUA_OK);
        /* Extend coroutine frame to hold remaining arguments. */
        rtop = rbase + NARGS - (OP == 0x6f); /* resume: -1 for `co' */
        /* Make sure we don't exceed the coroutine’s stack space. */
        if (rtop > mref(co->maxstack, TValue))
          goto resume_fallback;
        else
          co->top = rtop;
        /* Save caller PC. */
        vm_savepc(L, (BCIns*)BASE[-1].u64);
        /* Clear arguments from stack. */
        TOP = BASE;
        if (OP == 0x6f)
          /* resume: keep resumed thread in parent stack for GC. */
          TOP += 1;
        /* Copy arguments. resume: -1 for `co' */
        copyTVs(L, rbase, TOP, rtop-rbase, NARGS - (OP == 0x6f));
        /* Resume coroutine at rbase. */
        lj_vm_resume(co, rbase, 0, 0);
        /* Reference the now-current lua_State. */
        setgcref(G(L)->cur_L, obj2gco(L));
        /* Handle result depending on co->status. */
        if (co->status <= LUA_YIELD) {
          /* Coroutine yielded with results. */
          int nresults = co->top - co->base;
          /* Clear coroutine stack. */
          co->top = co->base;
          /* Ensure we have stack space for coroutine results. */
          assert(TOP+nresults <= mref(L->maxstack, TValue));
          /* Copy coroutine results */
          copyTVs(L, TOP, co->base, nresults, nresults);
          if (OP==0x6f) {
            /* resume: prepend true to results. */
            setboolV(BASE, 1);
            nresults += 1;
          }
          vm_return(L, BASE[-1].u64, 0, nresults);
        } else {
          /* Coroutine returned with error (at co->top-1). */
          if (OP == 0x70) {
            /* wrap_aux: throw error. */
            lj_ffh_coroutine_wrap_err(L, co);
          } else {
            /* resume: catch the error. */
            co->top -= 1; /* Clear error from coroutine stack. */
            /* Return (false, <error>) */
            setboolV(BASE, 0);
            copyTV(L, BASE+1, co->top);
            if (vm_return(L, BASE[-1].u64, 0, 2)) return;
          }
        }
        break;
      resume_fallback:
        if (fff_fallback(L)) return;
        break;
      }
    case 0x71:
      TRACEFF("math.abs");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, BASE->n < 0 ? -1*BASE->n : BASE->n);
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x72:
      TRACEFF("math.floor");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, lj_vm_floor(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x73:
      TRACEFF("math.ceil");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, lj_vm_ceil(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x74:
      TRACEFF("math.sqrt");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, sqrt(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x75:
      TRACEFF("math.log10");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, log10(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x76:
      TRACEFF("math.exp");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, exp(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x77:
      TRACEFF("math.sin");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, sin(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x78:
      TRACEFF("math.cos");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, cos(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x79:
      TRACEFF("math.tan");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, tan(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x80:
      TRACEFF("math.frexp");
      if (NARGS >= 1 && tvisnum(BASE)) {
        int n;
        setnumV(BASE, frexp(numV(BASE), &n));
        setnumV(BASE+1, n);
        if (vm_return(L, BASE[-1].u64, 0, 2)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x81:
      TRACEFF("math.modf");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE+1, modf(numV(BASE), &BASE->n));
        if (vm_return(L, BASE[-1].u64, 0, 2)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x82:
      TRACEFF("math.log");
      if (NARGS >= 1 && tvisnum(BASE)) {
        setnumV(BASE-2, log(numV(BASE)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x83:
      TRACEFF("math.atan2");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        setnumV(BASE-2, atan2(numV(BASE), numV(BASE+1)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x86:
      TRACEFF("math.ldexp");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        setnumV(BASE-2, ldexp(numV(BASE), numV(BASE+1)));
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x87:
      TRACEFF("math.min");
      if (NARGS < 1 || !tvisnum(BASE))
        goto min_fallback;
      while (NARGS-- > 1) {
        if (!tvisnum(BASE+NARGS)) goto min_fallback;
        setnumV(BASE, min(numV(BASE), numV(BASE+NARGS)));
      }
      if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      break;
    min_fallback:
      if (fff_fallback(L)) return;
      break;
    case 0x88:
      TRACEFF("math.max");
      if (NARGS < 1 || !tvisnum(BASE))
        goto max_fallback;
      while (NARGS-- > 1) {
        if (!tvisnum(BASE+NARGS)) goto max_fallback;
        setnumV(BASE, max(numV(BASE), numV(BASE+NARGS)));
      }
      if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      break;
    max_fallback:
      if (fff_fallback(L)) return;
      break;
    case 0x89:
      TRACEFF("bit.tobit");
      if (NARGS >= 1 && tvisnum(BASE)) {
        BASE->n = tobit(BASE);
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x8a:
      TRACEFF("bit.bnot");
      if (NARGS >= 1 && tvisnum(BASE)) {
        BASE->n = ~tobit(BASE);
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x8b:
      TRACEFF("bit.bswap");
      if (NARGS >= 1 && tvisnum(BASE)) {
        BASE->n = (int32_t)bswap_32((uint32_t)tobit(BASE));
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x8c:
      TRACEFF("bit.lshift");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        BASE->n = tobit(BASE) << tobit(BASE+1);
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x8d:
      TRACEFF("bit.rshift");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        BASE->n = (int32_t)((uint32_t)tobit(BASE) >> tobit(BASE+1));
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x8e:
      TRACEFF("bit.arshift");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        BASE->n = tobit(BASE) >> tobit(BASE+1);
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x8f:
      TRACEFF("bit.rol");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        uint32_t b = tobit(BASE), n = (uint32_t)tobit(BASE+1) & 31;
        BASE->n = (int32_t)((b << n) | (b >> (32-n)));
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x90:
      TRACEFF("bit.ror");
      if (NARGS >= 2 && tvisnum(BASE) && tvisnum(BASE+1)) {
        uint32_t b = tobit(BASE), n = (uint32_t)tobit(BASE+1) & 31;
        BASE->n = (int32_t)((b << (32-n)) | (b >> n));
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x91:
      TRACEFF("bit.band");
      {
        if (NARGS < 1 || !tvisnum(BASE))
          goto band_fallback;
        int32_t res = tobit(BASE);
        while (NARGS-- > 1) {
          if (!tvisnum(BASE+NARGS)) goto band_fallback;
          res &= tobit(BASE+NARGS);
        }
        BASE->n = res;
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
        break;
      band_fallback:
        if (fff_fallback(L)) return;
        break;
      }
    case 0x92:
      TRACEFF("bit.bor");
      {
        if (NARGS < 1 || !tvisnum(BASE))
          goto bor_fallback;
        int32_t res = tobit(BASE);
        while (NARGS-- > 1) {
          if (!tvisnum(BASE+NARGS)) goto band_fallback;
          res |= tobit(BASE+NARGS);
        }
        BASE->n = res;
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
        break;
      bor_fallback:
        if (fff_fallback(L)) return;
        break;
      }
    case 0x93:
      TRACEFF("bit.bxor");
      {
        if (NARGS < 1 || !tvisnum(BASE))
          goto bxor_fallback;
        int32_t res = tobit(BASE);
        while (NARGS-- > 1) {
          if (!tvisnum(BASE+NARGS)) goto band_fallback;
          res ^= tobit(BASE+NARGS);
        }
        BASE->n = res;
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
        break;
      bxor_fallback:
        if (fff_fallback(L)) return;
        break;
      }
    case 0x94:
      TRACEFF("string.byte");
      if (NARGS >= 1 && tvisstr(BASE)
          && (NARGS == 1 || tvisnum(BASE+1))
          && (NARGS == 2 || tvisnum(BASE+2))) {
        GCstr *str = strV(BASE);
        int start = NARGS >= 2 ? numV(BASE+1) : 1;
        int end = NARGS >= 3 ? numV(BASE+2) : start;
        int i, nresults;
        if (start < 0)
          start = max(start + (int)str->len+1, 1);
        else
          start = max(min(start, (int)str->len+1), 1);
        if (end < 0)
          end = max(end + (int)str->len+1, 0);
        else
          end = min(end, (int)str->len);
        nresults = max(1+end-start, 0);
        assert(BASE+nresults <= mref(L->maxstack, TValue));
        for (i=0; i+start <= end; i++)
          setnumV(BASE+i, strdata(str)[i+start-1]);
        if (vm_return(L, BASE[-1].u64, 0, nresults)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x95:
      TRACEFF("string.char");
      /* XXX - punt to fallback. */
      fff_fallback(L);
      break;
    case 0x96:
      TRACEFF("string.sub");
      vm_savepc(L, PC);
      lj_gc_check(L);
      if (NARGS >= 2 && tvisstr(BASE) && tvisnum(BASE+1)
          && (NARGS == 2 || tvisnum(BASE+2))) {
        GCstr *str = strV(BASE);
        int start = numV(BASE+1);
        int end = NARGS > 2 ? numV(BASE+2) : -1;
        if (start < 0)
          start = max(start + (int)str->len+1, 1);
        else
          start = max(min(start, (int)str->len+1), 1);
        if (end < 0)
          end = max(end + (int)str->len+1, 0);
        else
          end = min(end, (int)str->len);
        str = lj_str_new(L, strdata(str)+start-1, max(1+end-start, 0));
        setgcVraw(BASE-2, (GCobj *)str, LJ_TSTR);
        if (vm_return(L, BASE[-1].u64, -2, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    case 0x97:
    case 0x98:
    case 0x99:
      /* Fast function string operations. */
      vm_savepc(L, PC);
      lj_gc_check(L);
      if (NARGS >= 1 && tvisstr(BASE)) {
        GCstr *str = strV(BASE);
        SBuf *buf = &G(L)->tmpbuf;
        buf->L = L;
        buf->p = buf->b;
        switch ((uint32_t)OP) {
        case 0x97:
          TRACEFF("string.reverse");
          lj_buf_putstr_reverse(buf, str);
          break;
        case 0x98:
          TRACEFF("string.lower");
          lj_buf_putstr_lower(buf, str);
          break;
        case 0x99:
          TRACEFF("string.upper");
          lj_buf_putstr_upper(buf, str);
          break;
        default: assert(0 && "NYI: fast string operation");
        }
        setgcVraw(BASE, (GCobj *)lj_buf_tostr(buf), LJ_TSTR);
        if (vm_return(L, BASE[-1].u64, 0, 1)) return;
      } else if (fff_fallback(L)) return;
      break;
    default: assert(0 && "INVALID BYTECODE");
    }
  }
  /* Tail recursion. */
  goto execute;
}


/* -- Call handling ------------------------------------------------------- */

/* Call Lua function or callable object.
 *
 * Setup new BASE for callee frame, set NARGS, and construct frame link
 * according to frame type. Set PC to beginning of function or __call
 * metamethod. In the latter case, the "function" is inserted as the first
 * argument.
 *
 * Note: when the frame type (`ftp') is FRAME_LUA then the current PC is used
 * as the return bytecode. For other frame types a delta link is computed based
 * on the offset between BASE and `callbase'.
 */
static inline void vm_call(lua_State *L, TValue *callbase, int _nargs, int ftp) {
  TValue *f = callbase-2;
  int delta = callbase - BASE;
  if (!tvisfunc(f)) {
    vm_savepc(L, PC);
    lj_meta_call(L, f, callbase + _nargs);
    _nargs += 1;
    assert(KBASE != callbase && "NYI: vm_call __call in tailcall.");
  }
  BASE = callbase;
  NARGS = _nargs;
  TOP = BASE + _nargs;
  BASE[-1].u64 = (ftp == FRAME_LUA) ? (intptr_t)PC : (delta << 3) + ftp;
  PC = mref(funcV(f)->l.pc, BCIns);
}

/* Perform a tailcall.
 *
 * Copies function and arguments at offset into current (parent) frame and
 * performs call via vm_call.
 */
static inline void vm_callt(lua_State *L, int offset, int _nargs) {
  TValue *callbase = BASE+2+offset;
  uint64_t link = BASE[-1].u64;
  if ((link & FRAME_TYPEP) == FRAME_VARG) {
    /* Frame below is a VARG frame, relocate BASE. */
    BASE -= link >> 3;
    link = BASE[-1].u64;
  }
  // Copy function and arguments down into parent frame.
  BASE[-2] = callbase[-2];
  copyTVs(L, BASE, callbase, NARGS, NARGS);
  vm_call(L, BASE, NARGS, FRAME_LUA);
  /* Replace frame link set by vm_call with parent link. */
  BASE[-1].u64 = link;
  /* Tailcall to a fast function? */
  if (funcV(BASE-2)->l.ffid > FF_C)
    if ((link & FRAME_TYPE) != FRAME_LUA) {
      /* Frame below is a C frame. Need to set constant pool address. */
      GCproto *pt = (GCproto*)((intptr_t)(PC-1) - sizeof(GCproto));
      KBASE = mref(pt->k, void);
    }
}


/* -- Return handling ----------------------------------------------------- */

/* Return to the previous frame.
 *
 * Ensures that...
 *   BASE is restored to the base frame of the previous stack frame.
 *   Return values are copied to BASE..BASE+NRESULTS+MULTRES.
 *
 * Returns true if the virtual machine should 'return' on the C stack
 * i.e. if we are returning from this invocation of the bytecode interpreter.
 */
static int vm_return(lua_State *L, uint64_t link, int resultofs, int nresults) {
  switch (link & FRAME_TYPE) {
  case FRAME_C:
    /* Returning from the VM to C code. */
    {
      CFrame *cf = cframe_raw(L->cframe);
      int nexpected = cf->nresults;
      int delta = link>>3;
      TValue *dst = BASE + resultofs;
      STATE = ~LJ_VMST_C;
      if (nexpected < 0) // Return all results.
        MULTRES = nexpected = nresults;
      /* Copy results into caller frame */
      dst = copyTVs(L, BASE-2, dst, nexpected, nresults);
      TOP = dst; // When returning from C frames, last result is the new TOP.
      BASE -= delta;
      return 1;
    }
    break;
  case FRAME_LUA:
    /* Return from a Lua function. */
    PC = (BCIns*)link;
    {
      /* Find details in caller's CALL instruction operands. */
      int delta = bc_a(*(PC-1));
      int nexpected = bc_b(*(PC-1)) - 1;
      GCproto *pt;
      if (nexpected < 0) // Return all results.
        MULTRES = nexpected = nresults;
      copyTVs(L, BASE-2, BASE+resultofs, nexpected, nresults);
      BASE -= 2 + delta;
      pt = funcproto(funcV(BASE-2));
      TOP = BASE + pt->framesize;
      KBASE = mref(pt->k, void);
      return 0;
    }
    break;
  case FRAME_VARG:
    /* Return from vararg function: relocate BASE down and resultofs up. */
    {
      int delta = link >> 3;
      BASE -= delta;
      return vm_return(L, BASE[-1].u64, resultofs + delta, nresults);
    }
    break;
  }
  switch (link & FRAME_TYPEP) {
  case FRAME_PCALL:
  case FRAME_PCALLH:
    /* Return from protected call: signal success to caller.
       (See lj_unwind_ff for unwinding from failed pcalls.) */
    {
      /* Pop pcall frame, and adjust resultofs accordingly.
       *
       * Decrement resultofs by one, and increment nresults by one.
       * Push TRUE for successful return from a pcall in front of results.
       * (We know there is space because we freed >= two slots from the pcall
       * frame.)
       *
       * Return from call frame with the adjusted resultofs/nresults.
       */
      int delta = link>>3;
      intptr_t nextlink = BASE[-delta-1].u64; /* Might be clobbered. */
      BASE -= delta; resultofs += delta;
      resultofs--; nresults++; setboolV(BASE+resultofs, 1);
      return vm_return(L, nextlink, resultofs, nresults);
    }
    break;
  case FRAME_CONT:
    /* Return from metamethod continuation frame: restore PC and caller frame,
       save L and delta for continuation, and invoke continuation. */
    {
      /* Note the FRAME_CONT layout:
       *                                           CONT_BASE
       *      -4       -3         -2         -1         0       1
       *  | <cont> |  <pc>  |<metamethod>| <link> | <arg1> |   ...
       */
      GCproto *pt;
      void (*cont)(void) = contptr(BASE[-4].u64);
      int delta = link>>3;
      PC = mref(BASE[-3].u64, BCIns);
      BASE -= delta;
      if ((intptr_t)cont == LJ_CONT_TAILCALL) {
        /* Tail call from C function. */
        vm_callt(L, -2, NARGS);
      } else if ((intptr_t)cont == LJ_CONT_FFI_CALLBACK) {
        /* Return from FFI callback. */
        assert(0 && "NYI: vm_return from FFI callback.");
      } else {
        /* Call continuation. */
        CONT_L = L;
        CONT_BASE = BASE+delta;
        CONT_OFS = resultofs;
        MULTRES = nresults;
        pt = funcproto(funcV(BASE-2));
        TOP = BASE + pt->framesize;
        KBASE = mref(pt->k, void);
        (*cont)();
      }
      return 0;
    }
    break;
  }
  assert(0 && "NYI: Unsupported case in vm_return");
  return 0;
}


/* -- Calling metamethod continuations ------------------------------------ */

/* Push continuation frame and call metamethod at `newbase'.
 * See vm_return for handling of FRAME_CONT.
 */
static inline void vm_call_cont(lua_State *L, TValue *newbase, int _nargs) {
  newbase[-3].u64 = (intptr_t)PC;
  vm_call(L, newbase, _nargs, FRAME_CONT);
}


/* -- Fast ASM functions--------------------------------------------------- */

/* Fallback to ASM fast function handler
 *
 * Call fallback handler for ASM fast functions (relics from the ASM VM) and
 * massage VM state to return to caller.
 *
 * This can not just use vm_return and needs a special handler because ASM fast
 * functions are peculiar in multiple ways:
 *  - they use the stack space BASE-2..BASE+NARGS
 *  - they can yield to retries and tailcalls
 *
 * Returns the value of vm_return, potentially signaling the caller to return
 * to a C frame.
 */
int fff_fallback(lua_State *L) {
  uint64_t link = BASE[-1].u64;
  TOP = BASE + NARGS;
  assert(TOP+1+LUA_MINSTACK <= mref(L->maxstack, TValue));
  lua_CFunction *f = &funcV(BASE-2)->c.f; /* C function pointer */
  vm_savepc(L, (BCIns*)link);
  int res = (*f)(L);
  switch (res) {
  case -1: /* FFH_TAILCALL */
    if ((link & FRAME_TYPE) == FRAME_LUA) {
      int delta = bc_a(*(BCIns*)link);
      BASE -= delta+2;
      PC = (BCIns*)BASE[-1].u64; /* Reset PC for debug_framepc(). */
      vm_callt(L, delta, NARGS);
    } else {
      assert(0 && "NYI: fff_fallback tail call in non-Lua frame.");
    }
    return 0;
  case  0: /* FFH_RETRY */
    PC--; /* Retry the operation. */
    return 0;
  default: /* FFH_RES(n) */
    return vm_return(L, link, -2, res-1); /* res is number of results + 1 */
  }
}


/* -- Hook and recording dispatch ----------------------------------------- */

/* Invoke hooks for or record the next instruction to be executed. */
static inline void vm_dispatch(lua_State *L) {
  global_State *g = G(L);
  uint8_t mode = g->dispatchmode;
  if (hook_active(g) & HOOK_EVENTMASK)
    assert(0 && "NYI: active hooks");
  if (mode & DISPMODE_REC && bc_op(*PC) < GG_LEN_SDISP) {
    /* NB: cframe->multres is used by lj_dispatch_ins. */
    ((CFrame *) cframe_raw(L->cframe))->multres = MULTRES + 1;
    lj_dispatch_ins(L, PC+1);
  } else if (mode & DISPMODE_CALL)
    lj_dispatch_call(L, PC+1);
}


/* -- JIT trace recorder -------------------------------------------------- */

/* Hot loop detection: invoke trace recorder if loop body is hot. */
static inline void vm_hotloop(lua_State *L) {
  /* Hotcount if JIT is on, but not while recording. */
  if ((G(L)->dispatchmode & (DISPMODE_JIT|DISPMODE_REC)) == DISPMODE_JIT) {
    HotCount old_count = hotcount_get(L2GG(L), PC);
    HotCount new_count = hotcount_set(L2GG(L), PC, old_count - HOTCOUNT_LOOP);
    if (new_count > old_count) {
      /* Hot loop counter underflow. */
      jit_State *J = L2J(L);
      vm_savepc(L, PC);
      J->L = L;
      lj_trace_hot(J, PC);
    }
  }
}

/* Hot call detection: invoke trace recorder if function is hot. */
static inline void vm_hotcall(lua_State *L) {
  /* Hotcount if JIT is on, but not while recording. */
  if ((G(L)->dispatchmode & (DISPMODE_JIT|DISPMODE_REC)) == DISPMODE_JIT) {
    HotCount old_count = hotcount_get(L2GG(L), PC);
    HotCount new_count = hotcount_set(L2GG(L), PC, old_count - HOTCOUNT_CALL);
    if (new_count > old_count) {
      /* Hot call counter underflow. */
      vm_savepc(L, PC);
      TOP = BASE + NARGS;
      uintptr_t hotcall = (uintptr_t)PC | 1; /* LSB set: marker for hot call. */
      lj_dispatch_call(L, (BCIns *)hotcall);
      vm_savepc(L, 0); /* Invalidate for subsequent line hook. */
    }
  }
}

/* Execute a JIT compiled machine code trace.
 *
 * Sets up the global context state needed for trace execution, and
 * synchronizes trace machine code and bytecode interpreter VM via
 * TraceCallState.
 *
 * Can either rethrow an error returned from the trace, or continue
 * interpreter execution with new PC/BASE.
 */
static inline void vm_exec_trace(lua_State *L, BCReg traceno) {
  jit_State *J = L2J(L);
  GCtrace *trace = gcrefp(J->trace[traceno], GCtrace);
  TraceCallState tcs = {};
  int status;
  GCfunc *fn;
  uint64_t link;
  BCIns *retpc;
  int delta;
  /* Setup trace context. */
  J2G(J)->jit_base = BASE;
  J2G(J)->tmpbuf.L = L;
  J2GG(J)->tcs = &tcs;
  tcs.state.gpr[GPR_DISPATCH] = (intptr_t)J2GG(J)->dispatch;
  tcs.state.gpr[GPR_BASE] = (intptr_t)BASE;
  /* Call JIT compiled trace with call state. */
  lj_vm_trace_call(&tcs, trace->mcode);
  /* Handle trace exit. */
  if (tcs.handler != TRACE_EXIT_INTERP_NOTRACK) {
    /* Record which trace exited to the interpreter. */
    J2G(J)->lasttrace = STATE;
  }
  /* Restore interpreter state. */
  if (tcs.handler == TRACE_EXIT) {
    J->L = L;
    J->parent = STATE;
    J->exitno = tcs.exitno;
    STATE = ~LJ_VMST_EXIT;
    BASE = J2G(J)->jit_base;
    J2G(J)->jit_base = NULL;
    status = lj_trace_exit(J, &tcs.state);
    PC = cframe_pc(cframe_raw(L->cframe)); // set by lj_trace_exit
  } else {
    BASE = (TValue*)tcs.state.gpr[GPR_BASE];
    status = (int)tcs.state.gpr[GPR_RET];
    PC = (BCIns*)tcs.state.gpr[GPR_PC];
  }
  /* Restore NARGS, MULTRES. */
  if (status > 0) {
    /* Status is MULTRES+1. */
    NARGS = MULTRES = status-1;
  } else if (status < 0) {
    /* Error returned from trace, rethrow from the right C frame. */
    lj_err_throw(L, -status);
  }
  /* Restore KBASE. */
  fn = funcV(BASE-2);
  if (isluafunc(fn)) {
    KBASE = mref(funcproto(fn)->k, void);
  } else {
    link = BASE[-1].u64;
    if ((link & FRAME_TYPE) == FRAME_LUA) {
      /* Set KBASE for Lua function below. */
      retpc = (BCIns *)link;
      delta = bc_a(*(retpc-1));
      fn = funcV(BASE-delta-2-2);
      KBASE = mref(funcproto(fn)->k, void);
    } else {
      /* Trace stitching continuation. */
      assert((link & FRAME_TYPE) == FRAME_CONT);
    }
  }
  /* Return to interpreter. */
  J2G(J)->jit_base = NULL;
  STATE = ~LJ_VMST_INTERP;
}

/* Trace stitching continuation. */
void lj_cont_stitch(void) {
  lua_State *L = CONT_L;
  jit_State *J = L2J(L);
  BCIns curins = *(PC-1);
  GCtrace *prev = (GCtrace *)gcV(CONT_BASE-5);
  TValue *callbase = BASE+A;
  /* Copy results. */
  copyTVs(L, callbase, CONT_BASE+CONT_OFS, B-1, MULTRES);
  /* Have a trace, and it is not blacklisted? */
  if (prev && prev->traceno != prev->link) {
    if (prev->link) {
      /* Jump to stitched trace. */
      vm_exec_trace(L, prev->link);
    } else {
      /* Stitch a new trace to the previous trace. */
      J->L = L;
      J->exitno = prev->traceno;
      /* NB: cframe->multres is used by lj_dispatch_stitch. */
      ((CFrame *) cframe_raw(L->cframe))->multres = MULTRES + 1;
      lj_dispatch_stitch(J, PC);
    }
  }
}


/* -- Various auxiliary VM functions -------------------------------------- */

/* Save a pc to the active CFrame.
 *
 * Note: this needs to be called before calling out to external functions that
 * can throw Lua errors in order for them to be able to produce error messages.
 */
static inline void vm_savepc(lua_State *L, const BCIns *pc) {
  setcframe_pc(cframe_raw(L->cframe), pc);
}

/* Helper tobit function for bitops.
 *
 * Takes a Lua number `n' and produces a signed integer in the 32-bit result
 * range.
 */
static inline int32_t tobit(TValue *n) {
  static union {lua_Number n; uint64_t b;} bn;
  bn.n = n->n + 6755399441055744.0; /* 2^52+2^51 */
  return (int32_t)bn.b;
}


/* -- API functions ------------------------------------------------------- */

/* Call a Lua function from C. */
int luacall(lua_State *L, int p, TValue *newbase, int nres, ptrdiff_t ef)
{
  int res;
  const BCIns *oldpc = PC;
  /* Add new CFrame to the chain. */
  CFrame cf = { L->cframe, L, nres };
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  L2J(L)->L = L;
  /* Setup VM state for callee. */
  STATE = ~LJ_VMST_INTERP;
  vm_call(L, newbase, TOP - newbase, (p ? FRAME_CP : FRAME_C));
  /* Setup "catch" jump buffer for a protected call. */
  res = _setjmp(cf.jb);
  if (res <= 0) { /* -1 signals to continue from pcall, xpcall. */
    /* Try */
    execute(L);
  } else {
    /* Catch */
    return res;
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
  /* Restore PC. */
  PC = oldpc;
  return LUA_OK;
}

/* Call a Lua function object. */
void lj_vm_call(lua_State *L, TValue *newbase, int nres) {
  luacall(L, 0, newbase, nres-1, 0); /* -1 to compensate for lj_api +1 */
}

/* Call a Lua function object with a protected (error-handling) stack
 * frame.
 */
int lj_vm_pcall(lua_State *L, TValue *newbase, int nres, ptrdiff_t ef)  {
  return luacall(L, 1, newbase, nres-1, ef);
}

/* Call a C function with a protected (error-handling) stack frame. */
int lj_vm_cpcall(lua_State *L, lua_CFunction f, void *ud, lua_CPFunction cp) { 
  int res;
  const BCIns *oldpc = PC;
  TValue *newbase = NULL;
  /* "Neg. delta means cframe w/o frame." */
  int nresults = -savestack(L, L->top);
  /* Add to CFrame chain. */
  CFrame cf = { L->cframe, L, nresults };
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  L2J(L)->L = L;
  /* Save PC to new CFrame (needed for error handling in f). */
  vm_savepc(L, PC);
  /* Setup "catch" jump buffer for a protected call. */
  res = _setjmp(cf.jb);
  if (res < 0) {
    /* -1 signals to continue execution from pcall, xpcall. */
    execute(L);
  } else if (res == 0) {
    /* Try */
    newbase = cp(L, f, ud);
    if (newbase) {
      /* Setup VM state for callee. */
      STATE = ~LJ_VMST_INTERP;
      vm_call(L, newbase, L->top - newbase, FRAME_CP);
      execute(L);
    }
  } else {
    /* Catch */
    PC = oldpc;
    return res;
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
  PC = oldpc;
  return LUA_OK;
}

/* Resume coroutine, see ASM fast functions resume and yield.
 *
 * Note: nres1 is ignored, ef NYI.
 */
int lj_vm_resume(lua_State *L, TValue *newbase, int nres1, ptrdiff_t ef) {
  int res;
  const BCIns *oldpc = PC;
  /* Set CFrame. (Note: this frame is unlinked by yield.) */
  CFrame cf = { 0, L, -1 };
  setmref(L->cframe, (intptr_t)&cf + CFRAME_RESUME);
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  L2J(L)->L = L;
  /* Setup VM state for callee. */
  STATE = ~LJ_VMST_INTERP;
  if (L->status == LUA_OK) {
    /* Initial resume (like a call). */
    vm_call(L, newbase, TOP-newbase, FRAME_CP);
  } else {
    /* Resume after yield (like a return). */
    L->status = LUA_OK;
    vm_return(L, BASE[-1].u64, newbase-BASE, TOP-newbase);
  }
  /* Setup "catch" jump buffer for a protected call. */
  res = _setjmp(cf.jb);
  if (res <= 0) /* -1 signals to continue from pcall, xpcall. */
    /* Try */
    execute(L);
  else
    /* Catch */
    L->status = res;
  /* Restore PC. */
  PC = oldpc;
  return L->status;
}

/* Unwind from CFrame, longjmp with errcode. */
void lj_vm_unwind_c(void *cframe, int errcode) {
  longjmp(mref(cframe_raw(cframe), CFrame)->jb, errcode);
}

/* Unwind from protected Lua frame, see fast functions pcall and xpcall. */
void lj_vm_unwind_ff(void *cframe) {
  lua_State *L = mref(cframe_raw(cframe), CFrame)->L;
  uint64_t link = BASE[-1].u64;
  setboolV(BASE-1, 0); /* Push FALSE for unsuccessful return from a pcall.  */
  vm_return(L, link, -1, 2);
  lj_vm_unwind_c(cframe, -1); /* -1 < LUA_OK signals luacall to continue. */
}

void lj_vm_unwind_c_eh(void)                   { assert(0 && "NYI"); }
void lj_vm_unwind_ff_eh(void)                  { assert(0 && "NYI"); }
void lj_vm_unwind_rethrow(void)                { assert(0 && "NYI"); }
void lj_vm_ffi_callback()                      { assert(0 && "NYI"); }

/* Miscellaneous functions. */
int lj_vm_cpuid(uint32_t f, uint32_t res[4])       {
  asm volatile("cpuid":"=a"(*res),"=b"(*(res+1)),
               "=c"(*(res+2)),"=d"(*(res+3)):"a"(f));
  return (int)res[0];
}

/* Dispatch targets for recording and hooks. */
void lj_vm_record(void)   { assert(0 && "NYI"); }
void lj_vm_inshook(void)  { assert(0 && "NYI"); }
void lj_vm_rethook(void)  { assert(0 && "NYI"); }
void lj_vm_callhook(void) { assert(0 && "NYI"); }

/* Internal math helper functions. */
double lj_vm_floor(double a) { return floor(a); }
double lj_vm_ceil(double a)  { return ceil(a); }

void lj_vm_floor_sse(void)   { assert(0 && "NYI"); }
void lj_vm_ceil_sse(void)    { assert(0 && "NYI"); }
void lj_vm_trunc_sse(void)   { assert(0 && "NYI"); }
void lj_vm_powi_sse(void)    { assert(0 && "NYI"); }
double lj_vm_trunc(double d) { assert(0 && "NYI"); }

/* Continuations for metamethods. */
void lj_cont_cat(void) {
  /* Continue with concatenation. */
  lua_State *L = CONT_L;
  BCIns curins = *(PC-1);
  int left = (CONT_BASE-4) - (BASE+B);
  if (left > 0) {
    /* CAT has remaining arguments, concatenate. */
    copyTVs(L, CONT_BASE-4, CONT_BASE+CONT_OFS, 1, MULTRES);
    TValue *mbase = lj_meta_cat(L, CONT_BASE-4, left);
    if (mbase) vm_call_cont(L, mbase, 2);
    else copyTV(L, BASE+A, BASE+B);
  } else {
    /* CAT is complete, store result. */
    lj_cont_ra();
  }
}

void lj_cont_ra(void) {
  /* Store result in A from invoking instruction. */
  lua_State *L = CONT_L;
  BCIns curins = *(PC-1);
  copyTVs(L, BASE+A, CONT_BASE+CONT_OFS, 1, MULTRES);
}

void lj_cont_nop(void) {
  /* Do nothing, just continue execution. */
}

void lj_cont_condt(void) {
  /* Branch if result is true. */
  int flag = MULTRES && tvistruecond(CONT_BASE+CONT_OFS);
  BCIns curins = *PC++;
  if (flag) branchPC(D);
}

void lj_cont_condf(void)  {
  /* Branch if result is false. */
  int flag = !(MULTRES && tvistruecond(CONT_BASE+CONT_OFS));
  BCIns curins = *PC++;
  if (flag) branchPC(D);
}

void lj_cont_hook(void)	  { assert(0 && "NYI"); }

char lj_vm_asm_begin[0];

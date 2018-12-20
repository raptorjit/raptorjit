/*
** RaptorJIT virtual machine bytecode interpreter.
*/

#include <assert.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>

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

#define TRACE(name) printf("%-6s A=%-3d B=%-3d C=%-3d D=%-5d stackdepth=%d\n", \
                           name, A, B, C, D, TOP-BASE)

#define neg(n) (-1 - (n))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define min(a,b) ((a)<(b) ? (a) : (b))

/* Forward declaration. */
static int vm_return(lua_State *L, uint64_t link, int resultofs, int nresults);

/* Simple debug utility. */
#if 0
static void printstack(lua_State *L)
{
  int i;
  for (i = -2; i < L->top - L->base; i++) {
    TValue *v = L->base + i;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
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
static int multres;
static int nargs;
static void *kbase;
static const BCIns *pc;

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
 * MULTRES is set both by multiple value call instructions (e.g.
 * CALLM) and by multiple value return instructions (e.g. RETM).
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
  BCIns curins = *PC++;
  switch (OP) {
  case BC_ISLT:   assert(0 && "NYI BYTECODE: ISLT");
  case BC_ISGE:   assert(0 && "NYI BYTECODE: ISGE");
  case BC_ISLE:   assert(0 && "NYI BYTECODE: ISLE");
  case BC_ISGT:   assert(0 && "NYI BYTECODE: ISGT");
  case BC_ISEQV:  assert(0 && "NYI BYTECODE: ISEQV");
  case BC_ISNEV:  assert(0 && "NYI BYTECODE: ISNEV");
  case BC_ISEQS:  assert(0 && "NYI BYTECODE: ISEQS");
  case BC_ISNES:  assert(0 && "NYI BYTECODE: ISNES");
  case BC_ISEQN:  assert(0 && "NYI BYTECODE: ISEQN");
  case BC_ISNEN:  assert(0 && "NYI BYTECODE: ISNEN");
  case BC_ISEQP:  assert(0 && "NYI BYTECODE: ISEQP");
  case BC_ISNEP:  assert(0 && "NYI BYTECODE: ISNEP");
  case BC_ISTC:   assert(0 && "NYI BYTECODE: ISTC");
  case BC_ISFC:   assert(0 && "NYI BYTECODE: ISFC");
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
  case BC_ISF:    assert(0 && "NYI BYTECODE: ISF");
  case BC_ISTYPE: assert(0 && "NYI BYTECODE: ISTYPE");
  case BC_ISNUM:  assert(0 && "NYI BYTECODE: ISNUM");
  case BC_MOV:
    TRACE("MOV");
    /* MOV: A = dst; D = src */
    copyTV(L, BASE+A, BASE+D);
    break;
  case BC_NOT:    assert(0 && "NYI BYTECODE: NOT");
  case BC_UNM:    assert(0 && "NYI BYTECODE: UNM");
  case BC_LEN:    assert(0 && "NYI BYTECODE: LEN");
  case BC_ADDVN:  assert(0 && "NYI BYTECODE: ADDVN");
  case BC_SUBVN:  assert(0 && "NYI BYTECODE: SUBVN");
  case BC_MULVN:  assert(0 && "NYI BYTECODE: MULVN");
  case BC_DIVVN:  assert(0 && "NYI BYTECODE: DIVVN");
  case BC_MODVN:  assert(0 && "NYI BYTECODE: MODVN");
  case BC_ADDNV:  assert(0 && "NYI BYTECODE: ADDNV");
  case BC_SUBNV:  assert(0 && "NYI BYTECODE: SUBNV");
  case BC_MULNV:  assert(0 && "NYI BYTECODE: MULNV");
  case BC_DIVNV:  assert(0 && "NYI BYTECODE: DIVNV");
  case BC_MODNV:  assert(0 && "NYI BYTECODE: MODNV");
  case BC_ADDVV:  assert(0 && "NYI BYTECODE: ADDVV");
  case BC_SUBVV:  assert(0 && "NYI BYTECODE: SUBVV");
  case BC_MULVV:  assert(0 && "NYI BYTECODE: MULVV");
  case BC_DIVVV:  assert(0 && "NYI BYTECODE: DIVVV");
  case BC_MODVV:  assert(0 && "NYI BYTECODE: MODVV");
  case BC_POW:    assert(0 && "NYI BYTECODE: POW");
  case BC_CAT:
    TRACE("CAT");
    {
      TValue *res = lj_meta_cat(L, BASE + C, C-B);
      assert(res == NULL && "NYI: CAT metamethod");
      copyTV(L, BASE+A, BASE+B);
    }
    break;
  case BC_KSTR:
    setgcVraw(BASE+A, kgcref(D, GCobj), LJ_TSTR);
    break;
  case BC_KCDATA: assert(0 && "NYI BYTECODE: KCDATA");
  case BC_KSHORT:
    TRACE("KSHORT");
    /* BASE[A] = D */
    setnumV(BASE+A, D);
    break;
  case BC_KNUM:   assert(0 && "NYI BYTECODE: KNUM");
  case BC_KPRI:   assert(0 && "NYI BYTECODE: KPRI");
  case BC_KNIL:   assert(0 && "NYI BYTECODE: KNIL");
  case BC_UGET:   assert(0 && "NYI BYTECODE: UGET");
  case BC_USETV:  assert(0 && "NYI BYTECODE: USETV");
  case BC_USETS:  assert(0 && "NYI BYTECODE: USETS");
  case BC_USETN:  assert(0 && "NYI BYTECODE: USETN");
  case BC_USETP:  assert(0 && "NYI BYTECODE: USETP");
  case BC_UCLO:   assert(0 && "NYI BYTECODE: UCLO");
  case BC_FNEW:
    TRACE("FNEW");
    {
      GCproto *pt = kgcref(D, GCproto);
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCfunc *fn = lj_func_newL_gc(L, pt, parent);
      setgcVraw(BASE+A, fn, LJ_TFUNC);
    }
    break;
  case BC_TNEW:
    TRACE("TNEW");
    if (G(L)->gc.total > G(L)->gc.threshold) {
      lj_gc_step_fixtop(L);
    }
    {
      uint32_t asize = D & ((1<<11)-1);
      uint32_t hbits = D >> 11;
      GCtab *tab = lj_tab_new(L, asize, hbits);
      setgcVraw(BASE+A, tab, LJ_TTAB);
    }
    break;
  case BC_TDUP:   assert(0 && "NYI BYTECODE: TDUP");
  case BC_GGET:
    TRACE("GGET");
    /* A = _G[D] */
    {
      GCfunc *fn = funcV(BASE-2);
      GCtab *env = tabref(fn->l.env);
      GCstr *key = kgcref(D, GCstr);
      cTValue *tv = lj_tab_getstr(env, key);
      assert(tv && !tvisnil(tv));
      copyTV(L, BASE+A, tv);
      break;
    }
  case BC_GSET:   assert(0 && "NYI BYTECODE: GSET");
  case BC_TGETV:  assert(0 && "NYI BYTECODE: TGETV");
  case BC_TGETS:
    TRACE("TGETS");
    {
      TValue *o = BASE+B;
      cTValue *res;
      GCstr *key = kgcref(C, GCstr);
      if (tvistab(o)) {
        GCtab *tab = tabV(o);
        res = lj_tab_getstr(tab, key);
      } else {
        TValue tvkey;
        /* Convert key to tagged value. */
        setgcVraw(&tvkey, obj2gco(key), LJ_TSTR);
        // XXX SAVE_L
        // XXX SAVE_PC
        res = lj_meta_tget(L, o, &tvkey);
      }
      copyTV(L, BASE+A, res);
      break;
    }
  case BC_TGETB:  assert(0 && "NYI BYTECODE: TGETB");
  case BC_TGETR:  assert(0 && "NYI BYTECODE: TGETR");
  case BC_TSETV:  assert(0 && "NYI BYTECODE: TSETV");
  case BC_TSETS:  assert(0 && "NYI BYTECODE: TSETS");
  case BC_TSETB:  assert(0 && "NYI BYTECODE: TSETB");
  case BC_TSETM:
    TRACE("TSETM");
    {
      int i = 0, ix = ktv(D)->u32.lo;
      TValue *o = BASE+A-1;
      GCtab *tab = tabV(o);
      if (isblack((GCobj*)tab))
        lj_gc_barrierback(G(L), tab);
      if (tab->asize < ix+NARGS)
        lj_tab_reasize(L, tab, ix + NARGS);
      for (i = 0; i < NARGS; i++)
        copyTV(L, BASE+A+i, BASE+D+i);
    }
    break;
  case BC_TSETR:  assert(0 && "NYI BYTECODE: TSETR");
  case BC_CALLM:  assert(0 && "NYI BYTECODE: CALLM");
  case BC_CALL:
    /* CALL: A = newbase, B = nresults+1, C = nargs+1 */
    TRACE("CALL");
    {
      TValue *f;
      /* Setup new base for callee frame. */
      BASE += 2 + A;
      f = BASE-2;
      assert(tvisfunc(f) && "NYI: CALL to non-function");
      /* Notes:
       *
       * PC is 32-bit aligned and so the low bits are always 00 which
       * corresponds to the FRAME_LUA tag value.
       *
       * CALL does not have to record the number of expected results
       * in the frame data. The callee's RET bytecode will locate this
       * CALL and read the value from the B operand. */
      BASE[-1].u64 = (intptr_t)PC;
      PC = mref(funcV(f)->l.pc, BCIns);
      NARGS = C-1;
      break;
    }
    break;
  case BC_CALLMT: assert(0 && "NYI BYTECODE: CALLMT");
  case BC_CALLT:  assert(0 && "NYI BYTECODE: CALLT");
  case BC_ITERC:  assert(0 && "NYI BYTECODE: ITERC");
  case BC_ITERN:  assert(0 && "NYI BYTECODE: ITERN");
  case BC_VARG:
    TRACE("VARG");
    {
      int delta = BASE[-1].u64 >> 3;
      MULTRES = delta;
      copyTVs(L, BASE+A, BASE-2-delta+C, B>0 ? B : MULTRES, MULTRES);
    }
    break;
  case BC_ISNEXT: assert(0 && "NYI BYTECODE: ISNEXT");
  case BC_RETM:   assert(0 && "NYI BYTECODE: RETM");
  case BC_RET:    assert(0 && "NYI BYTECODE: RET");
  case BC_RET0:
    TRACE("RET0");
    {
      int resultofs = A;
      uint64_t link = BASE[-1].u64;
      MULTRES = D-1;
      switch (link & FRAME_TYPE) {
      case FRAME_LUA: assert(0 && "NYI: Return to Lua frame");
      case FRAME_CONT: assert(0 && "NYI: Return to Continuation frame");
      case FRAME_VARG: assert(0 && "NYI: Return to vararg frame");
      case FRAME_C:
        if (vm_return(L, link, resultofs, 0)) return;
      }
    }
    break;
  case BC_RET1:   assert(0 && "NYI BYTECODE: RET1");
  case BC_FORL:
    TRACE("FORL");
    break;        /* XXX hotloop */
  case BC_JFORI:  assert(0 && "NYI BYTECODE: JFORI");
  case BC__MAX:   assert(0 && "Illegal bytecode: BC__MAX"); /* Suppress warning */
  case BC_FORI:
    TRACE("FORI");
    {
      TValue *state = BASE + A;
      TValue *idx = state, *stop = state+1, *step = state+2, *ext = state+3;
      assert(tvisnum(idx)  && "NYI: non-number loop index");
      assert(tvisnum(stop) && "NYI: non-number loop stop");
      assert(tvisnum(step) && "NYI: non-number loop step");
      setnumV(ext, idx->n);
      /* Check for termination */
      if ((step->n >= 0 && idx->n >= stop->n) ||
          (step->n <  0 && stop->n >= idx->n)) {
        pc += bc_j(D);
      }
    }
    break;
  case BC_IFORL:  assert(0 && "NYI BYTECODE: IFORL");
  case BC_JFORL:  assert(0 && "NYI BYTECODE: JFORL");
  case BC_ITERL:  assert(0 && "NYI BYTECODE: ITERL");
  case BC_IITERL: assert(0 && "NYI BYTECODE: IITERL");
  case BC_JITERL: assert(0 && "NYI BYTECODE: JITERL");
  case BC_LOOP:   assert(0 && "NYI BYTECODE: LOOP");
  case BC_ILOOP:  assert(0 && "NYI BYTECODE: ILOOP");
  case BC_JLOOP:  assert(0 && "NYI BYTECODE: JLOOP");
  case BC_JMP:    assert(0 && "NYI BYTECODE: JMP");
  case BC_FUNCF:  assert(0 && "NYI BYTECODE: FUNCF");
  case BC_IFUNCF: assert(0 && "NYI BYTECODE: IFUNCF");
  case BC_JFUNCF: assert(0 && "NYI BYTECODE: JFUNCF");
  case BC_FUNCV:
    TRACE("FUNCV");
    {
      GCproto *pt = (GCproto*)((intptr_t)(PC-1) - sizeof(GCproto));
      /* Save base of frame containing all parameters. */
      TValue *oldbase = BASE;
      /* Base for new frame containing only fixed parameters. */
      BASE += 2 + NARGS;
      copyTV(L, BASE-2, oldbase-2);
      BASE[-1].u64 = FRAME_VARG + (NARGS << 3);
      copyTVs(L, BASE, oldbase, pt->numparams, NARGS);
      TOP = BASE + pt->framesize;
      /* Set constant pool address. */
      KBASE = mref(pt->k, void);
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
      assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
      assert(OP == BC_FUNCC); /* XXX */
      TOP = BASE + NARGS;
      STATE = ~LJ_VMST_C;
      nresults = (*f)(L);
      STATE = ~LJ_VMST_INTERP;
      resultofs = TOP - (BASE + nresults);
      if (vm_return(L, BASE[-1].u64, resultofs, nresults)) return;
    }
    break;
  }
  /* Tail recursion. */
  execute(L);
}


/* -- Return handling ----------------------------------------------------- */

/* Return to the previous frame.
 *
 * Requires that PC is loaded with the return address.
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
      CFrame *cf = (CFrame*)L->cframe;
      int delta = link>>3;
      TValue *dst = BASE - delta;
      /* Push TRUE for successful return from a pcall.  */
      if (link & FRAME_P) setboolV(dst++, 1);
      assert(cf->nresults>=0);
      STATE = ~LJ_VMST_C;
      /* Copy results into caller frame */
      dst = copyTVs(L, dst, BASE+resultofs, cf->nresults, nresults);
      TOP = dst + 1;
      BASE -= delta;
      return 1;
    }
    break;
  case FRAME_LUA:
    PC = (BCIns*)link;
    {
      assert(nresults>0 && "NYI: Multiple value call return");
      /* Find details in caller's CALL instruction operands. */
      int delta = bc_a(*(PC-1));
      int nexpected = bc_b(*(PC-1));
      GCproto *pt;
      copyTVs(L, BASE-2, BASE+resultofs, nexpected, nresults);
      BASE -= 2 + delta;
      pt = funcproto(funcV(BASE-2));
      TOP = BASE + pt->framesize;
      KBASE = mref(pt->k, void);
      return 0;
    }
    break;
  }
  assert(0 && "NYI: Unsupported case in vm_return");
  return 0;
}


/* -- API functions ------------------------------------------------------- */

/* Call a Lua function from C. */
int luacall(lua_State *L, int p, TValue *newbase, int nres, ptrdiff_t ef)
{
  int res;
  GCfunc *func;
  /* Add new CFrame to the chain. */
  CFrame cf = { L->cframe, L, nres };
  //assert(nres >= 0 && "NYI: LUA_MULTRET");
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  /* Set return frame link. */
  newbase[-1].u64 = (p ? FRAME_CP : FRAME_C) + ((newbase - BASE) << 3);
  /* Setup VM state for callee. */
  STATE = ~LJ_VMST_INTERP;
  NARGS = (TOP - newbase);
  BASE = newbase;
  TOP = BASE + NARGS;
  /* Branch and execute callee. */
  func = funcV(newbase-2);
  PC = mref(func->l.pc, BCIns);
  /* Setup "catch" jump buffer for a protected call. */
  if ((res = _setjmp(cf.jb)) == 0) {
    /* Try */
    execute(L);
  } else {
    /* Catch */
    assert(0 && "NYI: Catch exception from Lua call");
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
  /* XXX */
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
  /* "Neg. delta means cframe w/o frame." */
  int nresults = -savestack(L, L->top) / sizeof(TValue);
  /* Add to CFrame chain. */
  CFrame cf = { L->cframe, L, nresults };
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  if ((res = _setjmp(cf.jb)) == 0) {
    /* Try */
    TValue *newbase = cp(L, f, ud);
    if (newbase == NULL) {
      return 0;
    } else {
      return luacall(L, 0, newbase, nresults, 0);
    }      
  } else {
    /* Catch */
    assert(0 && "NYI cpcall error");
  }
  return 0;
}

int lj_vm_resume(lua_State *L, TValue *base, int nres1, ptrdiff_t ef) { assert(0 && "NYI"); }
void lj_vm_unwind_c(void *cframe, int errcode) { assert(0 && "NYI"); }
void lj_vm_unwind_ff(void *cframe)             { assert(0 && "NYI"); };
void lj_vm_unwind_c_eh(void)                   { assert(0 && "NYI"); }
void lj_vm_unwind_ff_eh(void)                  { assert(0 && "NYI"); }
void lj_vm_unwind_rethrow(void)                { assert(0 && "NYI"); }
void lj_vm_ffi_callback()                      { assert(0 && "NYI"); }
void lj_vm_ffi_call(CCallState *cc)            { assert(0 && "NYI"); }

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

/* Trace exit handling. */
void lj_vm_exit_handler(void) { assert(0 && "NYI"); }
void lj_vm_exit_interp(void)  { assert(0 && "NYI"); }

/* Internal math helper functions. */
double lj_vm_floor(double a)             { assert(0 && "NYI"); }
double lj_vm_ceil(double a)              { assert(0 && "NYI"); }

void lj_vm_floor_sse(void)   { assert(0 && "NYI"); }
void lj_vm_ceil_sse(void)    { assert(0 && "NYI"); }
void lj_vm_trunc_sse(void)   { assert(0 && "NYI"); }
void lj_vm_powi_sse(void)    { assert(0 && "NYI"); }
double lj_vm_trunc(double d) { assert(0 && "NYI"); }

/* Continuations for metamethods. */
void lj_cont_cat(void)	  { assert(0 && "NYI"); }
void lj_cont_ra(void)	  { assert(0 && "NYI"); }
void lj_cont_nop(void)	  { assert(0 && "NYI"); }
void lj_cont_condt(void)  { assert(0 && "NYI"); }
void lj_cont_condf(void)  { assert(0 && "NYI"); }
void lj_cont_hook(void)	  { assert(0 && "NYI"); }
void lj_cont_stitch(void) { assert(0 && "NYI"); }

char lj_vm_asm_begin[0];

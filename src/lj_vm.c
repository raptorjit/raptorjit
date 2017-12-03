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

/* Convenient alternative representation of BCIns. */
typedef struct VMIns {
  uint8_t op;
  uint8_t a;
  union {
    struct {
      uint8_t b;
      uint8_t c;
    };
    uint16_t d;
  };
} VMIns;

/* Chain of stack-allocated C frame descriptors. */
typedef struct CFrame {
  struct CFrame *save_cframe;
  BCIns *save_pc;
  lua_State *save_L;
  /* NYI: SAVE_ERRF */
  int save_nres;
} CFrame;

/* 
** Registers
*/
static global_State *gl;
static int multres;
static int nargs;
static const BCIns *pc;

/* Return to the previous frame.
** pc is already loaded with the return address.
** Return true if the VM bytecode interpreter should return.
*/
static int vm_return(lua_State *L) {
  printf("return pc = %p\n", pc);
  if ((intptr_t)pc & FRAME_P) {
    /* Returning from a protected call. Prepend true to the results. */
    setboolV(L->base--, 1);
    multres++;
  }
  switch (FRAME_TYPE & (intptr_t)pc) {
  case FRAME_C:
    /* Returning from the VM to C code. */
    {
      int i;
      TValue *retbase = (TValue *)((intptr_t)L->base - ((intptr_t)pc & ~FRAME_TYPEP));
      /* Move returned values down the stack. */
      for (i = 0; i < multres; i--) {
	retbase[i] = L->base[i];
      }
      L->base = retbase;
      return 1;
    }
    break;
  }
  assert(0);
  return 0;
}

void execute(lua_State *L) {
  VMIns *i = (VMIns *)pc;
  switch (i->op) {
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
  case BC_IST:    assert(0 && "NYI BYTECODE: IST");
  case BC_ISF:    assert(0 && "NYI BYTECODE: ISF");
  case BC_ISTYPE: assert(0 && "NYI BYTECODE: ISTYPE");
  case BC_ISNUM:  assert(0 && "NYI BYTECODE: ISNUM");
  case BC_MOV:    assert(0 && "NYI BYTECODE: MOV");
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
  case BC_CAT:    assert(0 && "NYI BYTECODE: CAT");
  case BC_KSTR:   assert(0 && "NYI BYTECODE: KSTR");
  case BC_KCDATA: assert(0 && "NYI BYTECODE: KCDATA");
  case BC_KSHORT: assert(0 && "NYI BYTECODE: KSHORT");
  case BC_KNUM:   assert(0 && "NYI BYTECODE: KNUM");
  case BC_KPRI:   assert(0 && "NYI BYTECODE: KPRI");
  case BC_KNIL:   assert(0 && "NYI BYTECODE: KNIL");
  case BC_UGET:   assert(0 && "NYI BYTECODE: UGET");
  case BC_USETV:  assert(0 && "NYI BYTECODE: USETV");
  case BC_USETS:  assert(0 && "NYI BYTECODE: USETS");
  case BC_USETN:  assert(0 && "NYI BYTECODE: USETN");
  case BC_USETP:  assert(0 && "NYI BYTECODE: USETP");
  case BC_UCLO:   assert(0 && "NYI BYTECODE: UCLO");
  case BC_FNEW:   assert(0 && "NYI BYTECODE: FNEW");
  case BC_TNEW:   assert(0 && "NYI BYTECODE: TNEW");
  case BC_TDUP:   assert(0 && "NYI BYTECODE: TDUP");
  case BC_GGET:   assert(0 && "NYI BYTECODE: GGET");
  case BC_GSET:   assert(0 && "NYI BYTECODE: GSET");
  case BC_TGETV:  assert(0 && "NYI BYTECODE: TGETV");
  case BC_TGETS:  assert(0 && "NYI BYTECODE: TGETS");
  case BC_TGETB:  assert(0 && "NYI BYTECODE: TGETB");
  case BC_TGETR:  assert(0 && "NYI BYTECODE: TGETR");
  case BC_TSETV:  assert(0 && "NYI BYTECODE: TSETV");
  case BC_TSETS:  assert(0 && "NYI BYTECODE: TSETS");
  case BC_TSETB:  assert(0 && "NYI BYTECODE: TSETB");
  case BC_TSETM:  assert(0 && "NYI BYTECODE: TSETM");
  case BC_TSETR:  assert(0 && "NYI BYTECODE: TSETR");
  case BC_CALLM:  assert(0 && "NYI BYTECODE: CALLM");
  case BC_CALL:   assert(0 && "NYI BYTECODE: CALL");
  case BC_CALLMT: assert(0 && "NYI BYTECODE: CALLMT");
  case BC_CALLT:  assert(0 && "NYI BYTECODE: CALLT");
  case BC_ITERC:  assert(0 && "NYI BYTECODE: ITERC");
  case BC_ITERN:  assert(0 && "NYI BYTECODE: ITERN");
  case BC_VARG:   assert(0 && "NYI BYTECODE: VARG");
  case BC_ISNEXT: assert(0 && "NYI BYTECODE: ISNEXT");
  case BC_RETM:   assert(0 && "NYI BYTECODE: RETM");
  case BC_RET:    assert(0 && "NYI BYTECODE: RET");
  case BC_RET0:   assert(0 && "NYI BYTECODE: RET0");
  case BC_RET1:   assert(0 && "NYI BYTECODE: RET1");
  case BC_FORI:   assert(0 && "NYI BYTECODE: FORI");
  case BC_JFORI:  assert(0 && "NYI BYTECODE: JFORI");
  case BC_FORL:   assert(0 && "NYI BYTECODE: FORL");
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
  case BC_FUNCV:  assert(0 && "NYI BYTECODE: FUNCV");
  case BC_IFUNCV: assert(0 && "NYI BYTECODE: IFUNCV");
  case BC_JFUNCV: assert(0 && "NYI BYTECODE: JFUNCV");
  case BC_FUNCCW: assert(0 && "NYI BYTECODE: FUNCCW");
  case BC_FUNCC:
    /* 
    ** Call C function.
    */
    {
      lua_CFunction *f = &funcV(&L->base[-2])->c.f; /* C function pointer */
      L->top = &L->base[nargs];
      assert(&L->top[LUA_MINSTACK] <= mref(L->maxstack, TValue));
      G(L)->vmstate = ~LJ_VMST_C;
      (*f)(L);
      G(L)->vmstate = ~LJ_VMST_INTERP;
      /* XXX G cur_L */
      pc = (const BCIns *)L->base[-1].u64;
      if (vm_return(L)) return;
    }
    break;
  }
  /* Tail recursion. */
  execute(L);
}

/* 
** Entry points declared in lj_vm.h
**/

void lj_vm_call_common(lua_State *L, TValue *base, int nres, int frametype)
{
}

/* Entry points for ASM parts of VM. */
void lj_vm_call(lua_State *L, TValue *base, int nres) {
  GCfunc *f = lj_lib_checkfunc(L, 1);
  CFrame cf;
  /* Add to CFrame chain. */
  cf.save_cframe = L->cframe;
  cf.save_L = L;
  cf.save_pc = NULL;
  cf.save_nres = nres;
  L->cframe = &cf;
  /* Setup C return PC with base address delta. */
  L->base = base;
  base[-1].u64 = FRAME_C + (intptr_t)base - (intptr_t)L->base;
  nargs = L->top - L->base;
  pc = (const BCIns *)f->l.pc.ptr64;
  G(L)->vmstate = ~LJ_VMST_INTERP;
  execute(L);
  L->cframe = cf.save_cframe;
}

int lj_vm_cpcall(lua_State *L, lua_CFunction f, void *ud, lua_CPFunction cp) { 
  CFrame cf;
  jmp_buf jb;
  int res;
  /* Add to CFrame chain. */
  cf.save_cframe = L->cframe;
  cf.save_L = L;
  cf.save_pc = NULL;
  /* cf.save_errf = NULL; */
  cf.save_nres = -savestack(L, L->top); /* "Neg. delta means cframe w/o frame." */
  L->cframe = &cf;
  /* Call with exception handler */
  if (1) {
  /* if ((res = _setjmp(jb)) == 0) { */
    /* Try */
    TValue *newbase;
    newbase = cp(L, f, ud);
    if (newbase) { L->base = newbase; }
    if (f) lj_lib_checkfunc(L, 1)->c.f(L);
    L->cframe = cf.save_cframe;
    return 0;
  } else {
    /* Catch */
    assert(0 && "NYI cpcall error");
  }
  return 0;
}

int lj_vm_pcall(lua_State *L, TValue *base, int nres1, ptrdiff_t ef)  { assert(0 && "NYI"); }

int lj_vm_resume(lua_State *L, TValue *base, int nres1, ptrdiff_t ef) { assert(0 && "NYI"); }
void lj_vm_unwind_c(void *cframe, int errcode) { assert(0 && "NYI"); }
void lj_vm_unwind_ff(void *cframe)             { assert(0 && "NYI"); }
void lj_vm_unwind_c_eh(void)                   { assert(0 && "NYI"); }
void lj_vm_unwind_ff_eh(void)                  { assert(0 && "NYI"); }
void lj_vm_unwind_rethrow(void)                { assert(0 && "NYI"); }
void lj_vm_ffi_callback()                      { assert(0 && "NYI"); }
void lj_vm_ffi_call(CCallState *cc)            { assert(0 && "NYI"); }

/* Miscellaneous functions. */
int lj_vm_cpuid(uint32_t f, uint32_t res[4])       { assert(0 && "NYI"); }

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

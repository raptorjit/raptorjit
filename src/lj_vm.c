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

/*
  XXX This seems to segfault when accessing the string data...
char *tostring(lua_State *L, TValue *v)
{
  if (!tvisnil(v) && !tvisstr(v))
    return strdata(lj_strfmt_obj(L, v));
  else
    return "...";
}
*/

/* Simple debug utility. */
void printstack(lua_State *L)
{
  int i;
  //printf("stackdump with %d elems\n", L->top - L->base);
  for (i = -2; i < L->top - L->base; i++) {
    TValue *v = L->base + i;
    //printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}

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
  int save_errf;
  int save_nres;
} CFrame;

/* 
** Registers
*/
static global_State *gl;
static int multres;
static int nargs;
static cTValue *kbase;
static const BCIns *pc;

/* Return to the previous frame.
** pc is already loaded with the return address.
** Return true if the VM bytecode interpreter should return.
*/
static int vm_return(lua_State *L, int resultofs, int nresults) {
  printf("returning to %p (%d)\n", pc, (intptr_t)pc>>3);
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
      TValue *src = L->base + resultofs;
      TValue *dst = L->base - ((intptr_t)pc>>3);
      int ncopy = nresults;
      CFrame *cf = (CFrame*)L->cframe;
      /* Copy results into caller frame */
      printf("delta = %d\n", ((intptr_t)pc>>3));
      while (ncopy-- > 0)
        copyTV(L, dst++, src++);
      G(L)->vmstate = ~LJ_VMST_C;
      /* More results wanted? Pad with nil. */
      while (multres < cf->save_nres) {
        setnilV(dst++);
        multres++;
      }
      /* Less results wanted? Prune stack. */
      if (multres > cf->save_nres)
        L->top -= multres - cf->save_nres;
      /* Restore base to caller frame. */
      L->base -= (intptr_t)pc>>3;
      /* Set stack top to last returned value. */
      L->top = dst + cf->save_nres + 2;
      printstack(L);
      return 1;
    }
    break;
  }
  assert(0);
  return 0;
}

void execute(lua_State *L) {
  VMIns *i = (VMIns *)pc++;
  printf("executing bc %d base %p top %p\n", i->op, L->base, L->top);
  //lj_gc_fullgc(L);              /* XXX */
  //printf("gc ok\n");
  printstack(L);
  assert(L->top >= L->base);
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
  case BC_KSHORT:
    //assert(L->base+i->a <= L->top);
    setnumV(L->base + i->a, i->d);
    printf("kshort %d = %d at %p readback %f\n", i->a, i->d, L->base+i->a, (L->base+i->a)->n);
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
  case BC_RET0:
    {
      VMIns *pcins;
      pc = (BCIns*)L->base[-1].u64;
      pcins = (VMIns *)pc;
      printf("pc = %p pcins = %p\n", pc, pcins);
      printf("fame tupe %x %x\n", (intptr_t)pc & FRAME_TYPE, FRAME_LUA);
      assert(((intptr_t)pc & FRAME_TYPE) != FRAME_LUA);
      if (vm_return(L, i->a, 0)) return;
      /*
      multres = i->d;
      numexpected = pcins->b;
      while (numexpected > multres)
        setnilV(L->base+(multres++-3));
      L->base -= pcins->a + 2;
      */
    }
    break;
  case BC_RET1:   assert(0 && "NYI BYTECODE: RET1");
    
  case BC_FORL:   break;        /* XXX hotloop */
  case BC_FORI:
    {
      TValue *state = L->base + i->a;
      TValue *idx = state, *stop = state+1, *step = state+2, *ext = state+3;
      printstack(L);
      printf("loop index %p %f\n", idx, idx->n);
      assert(tvisnum(idx)  && "NYI: non-number loop index");
      assert(tvisnum(stop) && "NYI: non-number loop stop");
      assert(tvisnum(step) && "NYI: non-number loop step");
      setnumV(ext, idx->n);
      /* Check for termination */
      if ((step->n >= 0 && idx->n >= stop->n) ||
          (step->n <  0 && stop->n >= idx->n)) {
        pc += bc_j(i->d);
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
    {
      int framesize = i->a;
      TValue *newbase = L->base + 2 + nargs;
      GCproto *pt = (GCproto*)((intptr_t)(pc-1) - sizeof(GCproto));
      // XXX kbase = mref(pt->k, cTValue);
      newbase[-1].u64 = FRAME_VARG + ((nargs+1) << 3);
      while (nargs < pt->numparams)
        setnilV(L->base+(nargs++)-1);
    }      
    break;
  case BC_IFUNCV: assert(0 && "NYI BYTECODE: IFUNCV");
  case BC_JFUNCV: assert(0 && "NYI BYTECODE: JFUNCV");
  case BC_FUNCCW: assert(0 && "NYI BYTECODE: FUNCCW");
  case BC_FUNCC:
    /* 
    ** Call C function.
    */
    {
      int nargs = i->d - 1;
      int nresults, resultofs;
      lua_CFunction *f = &funcV(L->base-2)->c.f; /* C function pointer */
      assert(&L->top[LUA_MINSTACK] <= mref(L->maxstack, TValue));
      assert(i->op == BC_FUNCC); /* XXX */
      printf("a n = %d m = %d nargs = %d\n", L->top - L->base, L->top - (TValue*)L->stack.ptr64, nargs);
      fflush(stdout);
      //L->top = L->base + nargs;
      printf("b n = %d m = %d\n", L->top - L->base, L->top - (TValue*)L->stack.ptr64);
      G(L)->vmstate = ~LJ_VMST_C;
      nresults = (*f)(L);
      printf("c n = %d m = %d\n", L->top - L->base, L->top - (TValue*)L->stack.ptr64);
      G(L)->vmstate = ~LJ_VMST_INTERP;
      pc = (const BCIns *)L->base[-1].u64;
      resultofs = L->top - (L->base + nresults);
      if (vm_return(L, resultofs, nresults)) return;
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
void lj_vm_call(lua_State *L, TValue *newbase, int nres) {
  GCfunc *func;
  CFrame cf;
  /* Add to CFrame chain. */
  cf.save_cframe = L->cframe;
  cf.save_L = L;
  cf.save_pc = NULL;
  cf.save_nres = nres;
  L->cframe = &cf;
  /* Setup C return PC with base address delta. */
  setgcref(G(L)->cur_L, obj2gco(L));
  G(L)->vmstate = ~LJ_VMST_INTERP;
  pc = (BCIns *)(FRAME_C + ((newbase - L->base) << 3));
  nargs = (L->top - newbase) + 1;
  func = funcV(newbase-2);
  //*((BCIns **)newbase-1) = (BCIns *)pc;
  newbase[-1].u64 = (uint64_t)pc;
  printf("newbase.0 = %p\n", newbase);
  pc = mref(func->l.pc, BCIns);
  L->base = newbase;
  L->top = L->base + nargs - 1;
  assert(L->top >= L->base);
  // XXX checkfunc LFUNC:RB, ->vmeta_call	// Ensure KBASE defined and != BASE.
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
  //cf.save_errf = NULL;
  cf.save_nres = -savestack(L, L->top) / sizeof(TValue); /* "Neg. delta means cframe w/o frame." */
  /* Call with exception handler */
  L->cframe = &cf;
  setgcref(G(L)->cur_L, obj2gco(L));
  if ((res = _setjmp(jb)) == 0) { /* Try */
    TValue *newbase = cp(L, f, ud);
    if (newbase == NULL) {
      return 0;
    } else {
      GCfunc *func;
      VMIns i;
      printf("newbase.1 = %p\n", newbase);
      printstack(L);
      setgcref(G(L)->cur_L, obj2gco(L));
      G(L)->vmstate = ~LJ_VMST_INTERP;
      /* PC = frame delta + frame type */
      pc = (BCIns *)(FRAME_CP + ((newbase - L->base) << 3));
      printf("delta = %d top = %p base = %p newbase = %p stack = %p pc = %p\n", newbase-L->base, L->top, L->base, newbase, L->stack);
      nargs = (L->top - newbase) + 1;
      // XXX checkfunc LFUNC:RB, ->vmeta_call	// Ensure KBASE defined and != BASE.
      func = funcV(newbase-2);
      L->base = newbase;
      L->base[-1].u64 = (uint64_t)pc;
      pc = mref(func->l.pc, BCIns);
      execute(L);
    }      
  } else {
    /* Catch */
    assert(0 && "NYI cpcall error");
  }
  return 0;
}

int lj_vm_pcall(lua_State *L, TValue *newbase, int nres1, ptrdiff_t ef)  {
  CFrame cf;
  GCfunc *func;
  cf.save_cframe = L->cframe;
  cf.save_errf = ef;
  cf.save_nres = nres1;
  cf.save_L = L;
  cf.save_pc = NULL;
  L->cframe = &cf;
  setgcref(G(L)->cur_L, obj2gco(L));
  G(L)->vmstate = ~LJ_VMST_INTERP;
  pc = (BCIns *)(FRAME_CP + ((newbase - L->base) << 3));
  nargs = (L->top - newbase) + 1;
  func = funcV(newbase-2);
  //*((BCIns **)newbase-1) = (BCIns *)pc;
  printf("newbase.2 = %p\n", newbase);
  newbase[-1].u64 = (uint64_t)pc;
  printf("saved pc = %p base = %p newbase = %p\n", pc, L->base, newbase);
  pc = mref(func->l.pc, BCIns);
  // XXX checkfunc LFUNC:RB, ->vmeta_call	// Ensure KBASE defined and != BASE.
  L->base = newbase;
  execute(L);
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

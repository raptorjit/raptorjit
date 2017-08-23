/*
** RaptorJIT virtual machine bytecode interpreter.
*/

#include <assert.h>
#include <stdint.h>

#include "lj_obj.h"
#include "lj_vm.h"
#include "lj_bc.h"
#include "lj_ccall.h"

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

void execute(global_State *g) {
  BCIns *pc = NULL;
  for (;;) {
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
    case BC_FUNCC:  assert(0 && "NYI BYTECODE: FUNCC");
    case BC_FUNCCW: assert(0 && "NYI BYTECODE: FUNCCW");
    }
  }
}

/* 
** Entry points declared in lj_vm.h
**/

/* Entry points for ASM parts of VM. */
void lj_vm_call(lua_State *L, TValue *base, int nres1)                { assert(0 && "NYI"); }
int lj_vm_pcall(lua_State *L, TValue *base, int nres1, ptrdiff_t ef)  { assert(0 && "NYI"); }
int lj_vm_cpcall(lua_State *L, lua_CFunction func, void *ud,
			 lua_CPFunction cp)                           { assert(0 && "NYI"); }
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

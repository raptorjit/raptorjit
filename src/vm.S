;;; vm96.S - RaptorJIT VM for x86-64 with 96-bit TValue size
;;;
;;; Tagged values (TValues) are represented as 96-bit words. The word
;;; is divided into a 32-bit "type" field and a 64-bit "value" field.
;;;
;;; +--32--+ +------64------+
;;; | TYPE | |    VALUE     |
;;; +------+ +--------------+
;;;
;;; The first 8 bits of the "type" field are an 8-bit internal type tag
;;; ("itype"). The itype value determines the meaning of all other
;;; bits (24 type bits and 64 value bits.)

#define BASE	edx	// (TValue *) Stack base of current function
#define KBASE	edi	// (cTValue *) Constants of current function
#define PC	esi	// (BCIns *) Next bytecode
#define DISPATCH ebx    // Opcode dispatch table

;;; Registers where bytecode instructions are unpacked.
;;; See bytecode ABC/AD layout in lj_bc.h.
#define RA	ecx
#define RAH	ch
#define RAL	cl
#define RAa	rcx
#define RB	ebp
#define RBa	rbp
#define RC	eax
#define RCW	ax
#define RCL	al
#define RCa	rax
#define RD	RC
#define RDW	RCW
#define RDL	RCL
#define RDa	RCa

;;; System V x86-64 calling convention
;;; See https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI
#define CARG1	rdi
#define CARG1d	edi
#define CARG2	rsi
#define	CARG2d	esi
#define CARG3	rdx
#define CARG3d	edx
#define CARG4	rcx
#define CARG4d	ecx
#define	CARG5	r8
#define	CARG5d	r8d
#define	CARG6	r9
#define CARG6d	r9d

;;; Type definition
;;; (GNU Assembler cannot understand C types?)

#define aword	8	// Machine address word size

#define CFRAME_SPACE	aword*5	// LJ"Delta for esp (see M<--)." XXX 5?
.macro saveregs_
	push rbx
	push r15
	push r14
	push r13
	push r12
	sub rsp, CFRAME_SPACE
.endmacro

.macro saveregs
	push rbp
	saveregs_
.endmacro

.macro restoreregs
	add rsp, CFRAME_SPACE
	pop r12
	pop r13
	pop r14
	pop r15
	pop rbx
	pop rbp
.endmacro
	

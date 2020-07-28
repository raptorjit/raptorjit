;;; FFI C function calling.
;;; Copyright (C) 2019 Max Rottenkolber. See Copyright Notice in luajit.h

global lj_vm_ffi_call

section .data

section .text

;;; Call a foreign C function with arguments and stack slots from a CCallState
;;; (see lj_ccall.h).
;;;
;;; Invokes C function according to System V ABI/Calling Convention for x86_64.
;;;
;;; This involves
;;;   - setting up the stack frame described in CCallState by adjusting the
;;;     stack pointer (rsp) and copying stack values from CCallState into the
;;;     new stack slots
;;;   - placing the function arguments in CCallState into general purpose and
;;;     floating point registers (GPRs and FPRs)
;;;
;;; After calling the C function
;;;   - we copy its return values (rax, rdx, xmm0, xmm1) back into CCallState
;;;   - and undo the stack pointer adjustment
;;;
lj_vm_ffi_call:
        push rbp
        mov rbp, rsp

        push rbx
        mov rbx, rdi            ;CCallState

;;; Readjust stack.
        mov eax, [rbx+8]        ;spadj
        sub rsp, rax

;;; Copy stack slots.
        movzx ecx, byte [rbx+12] ;nsp
        sub ecx, 1
        js .args
.stack:
        mov rax, [rbx+rcx*8+192] ;stack
        mov [rsp+rcx*8], rax
        sub ecx, 1
        jns .stack

;;; Put arguments into registers.
.args:
        movzx eax, byte [rbx+15] ;nfpr

        mov rdi, [rbx+0*8+144]  ;gpr[0]
        mov rsi, [rbx+1*8+144]
        mov rdx, [rbx+2*8+144]
        mov rcx, [rbx+3*8+144]
        mov  r8, [rbx+4*8+144]
        mov  r9, [rbx+5*8+144]
        test eax, eax
        jz .call
        movaps xmm0, [rbx+0*16+16] ;fpr[0]
        movaps xmm1, [rbx+1*16+16]
        movaps xmm2, [rbx+2*16+16]
        movaps xmm3, [rbx+3*16+16]
        cmp eax, 4
        jbe .call
        movaps xmm4, [rbx+4*16+16]
        movaps xmm5, [rbx+5*16+16]
        movaps xmm6, [rbx+6*16+16]
        movaps xmm7, [rbx+7*16+16]

.call:
        call qword [rbx+0]      ;func

;;; Copy results from GPRs and FPRs (registers) back into CCallState.
        mov [rbx+0*8+144], rax          ;gpr[0]
        movaps [rbx+0*16+16], xmm0      ;fpr[0]
        mov [rbx+1*8+144], rdx          ;gpr[1]
        movaps [rbx+1*16+16], xmm1      ;fpr[1]

;;; Pop stack frame and return to caller.
        mov rbx, [rbp-8]
        leave
        ret

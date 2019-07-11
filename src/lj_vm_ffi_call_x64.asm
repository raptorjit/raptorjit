global lj_vm_ffi_call

section .data

section .text

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

        mov [rbx+0*8+144], rax
        movaps [rbx+0*16+16], xmm0
        mov [rbx+1*8+144], rdx
        movaps [rbx+1*16+16], xmm1

        mov rbx, [rbp-8]
        leave
        ret

.extern printf
.global _start

.text
_start:
        mov $4, %rax
        push %rax

        mov $2, %rax
        push %rax

        pop %rcx
        pop %rax
        xor %rdx, %rdx
        idiv %rcx
        push %rax

        lea fmt(%rip), %rdi
        pop %rsi
        xor %eax, %eax
        call printf

        mov $60, %rax
        xor %rdi, %rdi
        syscall

.data
fmt:
        .asciz "%lld\n"

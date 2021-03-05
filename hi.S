.extern puts
.global _start

.text
_start:
        lea msg(%eip), %edi
        mov $puts, %rax
        call *%rax

        mov $60, %eax
        xor %edi, %edi
        syscall

msg:
        .asciz "Hi."

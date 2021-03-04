.extern puts
.global _start

.text
_start:
        lea msg(%eip), %edi
        call puts

        mov $60, %eax
        xor %edi, %edi
        syscall

msg:
        .asciz "Hi."

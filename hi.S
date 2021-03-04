.global _start

.text
_start:
        mov $1, %eax
        mov $1, %edi
        mov $msg, %esi
        mov $4, %edx
        syscall

        mov $60, %eax
        xor %edi, %edi
        syscall

msg:
        .asciz "Hi.\n"

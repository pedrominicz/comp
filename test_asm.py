#!/usr/bin/env python3

import sys

def write(number, size):
    sys.stdout.buffer.write(number.to_bytes(size, 'big'))

def zero():
    sys.stdout.buffer.write(b'\x00\x00\x00\x00')

def noop():
    sys.stdout.buffer.write(b'\x90')

# `load`
#   00000000  B800000000        mov eax,0x0
#   00000005  B900000000        mov ecx,0x0
#   0000000A  BA00000000        mov edx,0x0
#   0000000F  BB00000000        mov ebx,0x0
#   00000014  BC00000000        mov esp,0x0
#   00000019  BD00000000        mov ebp,0x0
#   0000001E  BE00000000        mov esi,0x0
#   00000023  BF00000000        mov edi,0x0
#   00000028  90                nop
for i in range(8):
    write(0xb8 + i, 1)
    zero()
noop()

#   00000029  0F94C0            setz al
#   0000002C  90                nop
write(0x0f92c0 + (2 << 8), 3)
noop()

# `cmp`
#   0000002D  4839C1            cmp rcx,rax
#   00000030  33C0              xor eax,eax
#   00000032  0F94C0            setz al
#   00000035  4839C1            cmp rcx,rax
#   00000038  33C0              xor eax,eax
#   0000003A  0F95C0            setnz al
#   0000003D  4839C1            cmp rcx,rax
#   00000040  33C0              xor eax,eax
#   00000042  0F9CC0            setl al
#   00000045  4839C1            cmp rcx,rax
#   00000048  33C0              xor eax,eax
#   0000004A  0F9DC0            setnl al
#   0000004D  4839C1            cmp rcx,rax
#   00000050  33C0              xor eax,eax
#   00000052  0F9EC0            setng al
#   00000055  4839C1            cmp rcx,rax
#   00000058  33C0              xor eax,eax
#   0000005A  0F9FC0            setg al
#   0000005D  90                nop
for i in [2, 3, 10, 11, 12, 13]:
    write(0x4839c1, 3)
    # Clear `eax` (`mov eax,0` on `qcc.ml`).
    write(0x33c0, 2)
    write(0x0f92c0 + (i << 8), 3)
noop()

# `test`
#   0000005E  4885C0            test rax,rax
#   00000061  0F8000000000      jo near 0x67
#   00000067  4885C0            test rax,rax
#   0000006A  0F8100000000      jno near 0x70
#   00000070  4885C0            test rax,rax
#   00000073  0F8200000000      jc near 0x79
#   00000079  4885C0            test rax,rax
#   0000007C  0F8300000000      jnc near 0x82
#   00000082  4885C0            test rax,rax
#   00000085  0F8400000000      jz near 0x8b
#   0000008B  4885C0            test rax,rax
#   0000008E  0F8500000000      jnz near 0x94
#   00000094  4885C0            test rax,rax
#   00000097  0F8600000000      jna near 0x9d
#   0000009D  4885C0            test rax,rax
#   000000A0  0F8700000000      ja near 0xa6
#   000000A6  4885C0            test rax,rax
#   000000A9  0F8800000000      js near 0xaf
#   000000AF  4885C0            test rax,rax
#   000000B2  0F8900000000      jns near 0xb8
#   000000B8  4885C0            test rax,rax
#   000000BB  0F8A00000000      jpe near 0xc1
#   000000C1  4885C0            test rax,rax
#   000000C4  0F8B00000000      jpo near 0xca
#   000000CA  4885C0            test rax,rax
#   000000CD  0F8C00000000      jl near 0xd3
#   000000D3  4885C0            test rax,rax
#   000000D6  0F8D00000000      jnl near 0xdc
#   000000DC  4885C0            test rax,rax
#   000000DF  0F8E00000000      jng near 0xe5
#   000000E5  4885C0            test rax,rax
#   000000E8  0F8F00000000      jg near 0xee
#   000000EE  90                nop
for i in range(16):
    write(0x4885c0, 3)
    # `0x0f84` on `qcc.ml`.
    write(0x0f80 + i, 2)
    zero()
noop()

# `push`
#   000000EF  50                push rax
#   000000F0  51                push rcx
#   000000F1  52                push rdx
#   000000F2  53                push rbx
#   000000F3  54                push rsp
#   000000F4  55                push rbp
#   000000F5  56                push rsi
#   000000F6  57                push rdi
#   000000F7  4150              push r8
#   000000F9  4151              push r9
#   000000FB  4152              push r10
#   000000FD  4153              push r11
#   000000FF  4154              push r12
#   00000101  4155              push r13
#   00000103  4156              push r14
#   00000105  4157              push r15
#   00000107  90                nop
for i in range(8):
    write(0x50 + i, 1)
for i in range(8):
    write(0x4150 + i, 2)
noop()

# `pop`
#   00000108  58                pop rax
#   00000109  59                pop rcx
#   0000010A  5A                pop rdx
#   0000010B  5B                pop rbx
#   0000010C  5C                pop rsp
#   0000010D  5D                pop rbp
#   0000010E  5E                pop rsi
#   0000010F  5F                pop rdi
#   00000110  4158              pop r8
#   00000112  4159              pop r9
#   00000114  415A              pop r10
#   00000116  415B              pop r11
#   00000118  415C              pop r12
#   0000011A  415D              pop r13
#   0000011C  415E              pop r14
#   0000011E  415F              pop r15
#   00000120  90                nop
for i in range(8):
    write(0x58 + i, 1)
for i in range(8):
    write(0x4158 + i, 2)
noop()

# `read`
#   00000121  488B00            mov rax,[rax]
#   00000124  480FB600          movzx rax,byte [rax]
#   00000128  90                nop
write(0x488b00, 3)
write(0x480fb600, 4)
noop()

#   00000129  480FAFC1          imul rax,rcx
#   0000012D  4891              xchg rax,rcx
#   0000012F  4899              cqo
#   00000131  48F7F9            idiv rcx
#   00000134  4891              xchg rax,rcx
#   00000136  4899              cqo
#   00000138  48F7F9            idiv rcx
#   0000013B  4892              xchg rax,rdx
#   0000013D  4801C8            add rax,rcx
#   00000140  4891              xchg rax,rcx
#   00000142  4829C8            sub rax,rcx
#   00000145  4891              xchg rax,rcx
#   00000147  48D3E0            shl rax,cl
#   0000014A  4891              xchg rax,rcx
#   0000014C  48D3F8            sar rax,cl
#   0000014F  4821C8            and rax,rcx
#   00000152  4831C8            xor rax,rcx
#   00000155  4809C8            or rax,rcx
#   00000158  90                nop
# `*`
write(0x480fafc1, 4)
# `/`
write(0x4891489948f7f9, 7)
# `%`
write(0x4891489948f7f94892, 9)
# `+`
write(0x4801c8, 3)
# `-`
write(0x48914829c8, 5)
# `<<`
write(0x489148d3e0, 5)
# `>>`
write(0x489148d3f8, 5)
# `&`
write(0x4821c8, 3)
# `^`
write(0x4831c8, 3)
# `|`
write(0x4809c8, 3)
noop()

write(0x488b45f8, 4)
write(0x488d45f8, 4)
write(0x488b3c24, 4)
write(0x488d3c24, 4)
noop()

write(0x4883ec00, 4)
write(0xe900000000, 5)

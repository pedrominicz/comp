#!/usr/bin/env python3

def read(string):
    result = []
    for word in string.split():
        try:
            result.append(int(word))
        except:
            assert len(word) == 1 and word in '+-*/()'
            result.append(word)
    return result

precedence = {'+': 0, '-': 0, '*': 1, '/': 1}

def convert(infix):
    postfix = []
    stack = []
    for item in infix:
        if type(item) is int:
            postfix.append(item)
        else:
            assert type(item) is str and len(item) == 1 and item in '+-*/()'
            if item == '(':
                stack.append(item)
            elif item == ')':
                while stack[-1] != '(':
                    postfix.append(stack.pop())
                # Discard '('
                stack.pop()
            else:
                while (stack and stack[-1] != '(' and
                       precedence[stack[-1]] >= precedence[item]):
                    postfix.append(stack.pop())
                stack.append(item)
    postfix.extend(stack[::-1])
    return postfix

print('.extern printf')
print('.global _start')
print('')
print('.text')
print('_start:')

for instruction in convert(read(input())):
    if type(instruction) is int:
        print(f'        mov ${instruction}, %rax')
        print(f'        push %rax')
    elif instruction == '+':
        print('        pop %rcx')
        print('        pop %rax')
        print('        add %rcx, %rax')
        print('        push %rax')
    elif instruction == '-':
        print('        pop %rcx')
        print('        pop %rax')
        print('        sub %rcx, %rax')
        print('        push %rax')
    elif instruction == '*':
        print('        pop %rcx')
        print('        pop %rax')
        print('        imul %rcx, %rax')
        print('        push %rax')
    elif instruction == '/':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cqo')
        print('        idiv %rcx')
        print('        push %rax')
    else:
        raise RuntimeError(f'unknown instruction: {instruction}')

print('        lea fmt(%rip), %rdi')
print('        pop %rsi')
print('        mov %rsi, %rdx')
print('        xor %eax, %eax')
print('        call printf')
print('')
print('        mov $60, %rax')
print('        xor %rdi, %rdi')
print('        syscall')
print('')
print('.data')
print('fmt:')
print('        .asciz "%lld\\n0x%016llx\\n"')

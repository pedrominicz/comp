#!/usr/bin/env python3

import sys

def read(string):
    result = []
    for word in string.split():
        try:
            result.append(int(word))
        except:
            result.append(word)
    return result

operators = [
    '(', ')', '*', '/', '%', '+', '-', '<<', '>>', '~', '!',
    '<', '<=', '>', '>=', '==', '!=', '&', '^', '|', '&&', '||', '='
]

# All binary operators are left associative.
precedence = {
    '*': 10, '/': 10, '%': 10,
    '+': 9, '-': 9,
    '<<': 8, '>>': 8,
    '<': 7, '<=': 7, '>': 7, '>=': 7,
    '==': 6, '!=': 6,
    '&': 5,
    '^': 4,
    '|': 3,
}

def convert(infix):
    postfix = []
    stack = []
    # Whether next token can be a prefix operator.
    prefix = True
    for token in infix:
        if type(token) is int:
            postfix.append(token)
            while stack and 'prefix' in stack[-1]:
                postfix.append(stack.pop())
            prefix = False
        else:
            assert token in operators
            if token == '(':
                stack.append(token)
                prefix = True
            elif token == ')':
                while stack[-1] != '(':
                    postfix.append(stack.pop())
                # Discard '('.
                stack.pop()
                while stack and 'prefix' in stack[-1]:
                    postfix.append(stack.pop())
                prefix = False
            elif prefix:
                stack.append(token + ' (prefix)')
                prefix = True
            else:
                while (stack and stack[-1] != '(' and
                       precedence[stack[-1]] >= precedence[token]):
                    postfix.append(stack.pop())
                stack.append(token)
                prefix = True
    postfix.extend(stack[::-1])
    return postfix

instructions = convert(read(input()))

sys.stderr.write(f'{instructions}\n')
sys.stderr.flush()

print('.extern printf')
print('.global _start')
print('')
print('.text')
print('_start:')

for instruction in instructions:
    if type(instruction) is int:
        print(f'        mov ${instruction}, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '*':
        print('        pop %rcx')
        print('        pop %rax')
        print('        imul %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '/':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cqo')
        print('        idiv %rcx')
        print('        push %rax')
        print('        nop')
    elif instruction == '%':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cqo')
        print('        idiv %rcx')
        print('        push %rdx')
        print('        nop')
    elif instruction == '+':
        print('        pop %rcx')
        print('        pop %rax')
        print('        add %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '-':
        print('        pop %rcx')
        print('        pop %rax')
        print('        sub %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '<<':
        print('        pop %rcx')
        print('        pop %rax')
        print('        sal %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '>>':
        print('        pop %rcx')
        print('        pop %rax')
        print('        sar %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '<':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cmp %rcx, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        setl %al')
        print('        push %rax')
        print('        nop')
    elif instruction == '<=':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cmp %rcx, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        setng %al')
        print('        push %rax')
        print('        nop')
    elif instruction == '>':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cmp %rcx, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        setg %al')
        print('        push %rax')
        print('        nop')
    elif instruction == '>=':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cmp %rcx, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        setnl %al')
        print('        push %rax')
        print('        nop')
    elif instruction == '==':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cmp %rcx, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        sete %al')
        print('        push %rax')
        print('        nop')
    elif instruction == '!=':
        print('        pop %rcx')
        print('        pop %rax')
        print('        cmp %rcx, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        setne %al')
        print('        push %rax')
        print('        nop')
    elif instruction == '&':
        print('        pop %rcx')
        print('        pop %rax')
        print('        and %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '^':
        print('        pop %rcx')
        print('        pop %rax')
        print('        xor %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '|':
        print('        pop %rcx')
        print('        pop %rax')
        print('        or %rcx, %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '+ (prefix)':
        pass
    elif instruction == '- (prefix)':
        print('        pop %rax')
        print('        neg %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '~ (prefix)':
        print('        pop %rax')
        print('        not %rax')
        print('        push %rax')
        print('        nop')
    elif instruction == '! (prefix)':
        print('        pop %rax')
        print('        test %rax, %rax')
        # Use `mov` because `xor` sets flags.
        print('        mov $0, %eax')
        print('        setz %al')
        print('        push %rax')
        print('        nop')
    else:
        raise RuntimeError(f'unknown instruction: {instruction}')

print('        lea fmt(%rip), %rdi')
print('        pop %rsi')
print('        mov %rsi, %rdx')
print('        mov $2, %al')
print('        call printf')
print('')
print('        mov $60, %rax')
print('        xor %edi, %edi')
print('        syscall')
print('')
print('.data')
print('fmt:')
print('        .asciz "%lld\\n0x%016llx\\n"')

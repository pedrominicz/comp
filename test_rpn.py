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
    result = []
    stack = []
    # Whether next token can be a prefix operator.
    prefix = True
    for token in infix:
        if token not in operators:
            result.append(token)
            while stack and 'prefix' in stack[-1]:
                result.append(stack.pop())
            prefix = False
        elif token == '(':
            stack.append(token)
            if not prefix:
                # A simple counter that increases when ',' is encountered and
                # resets when 'call' is encountered is enough figure to which
                # register ',' should pop.
                stack.append('call')
                stack.append(',')
            prefix = True
        elif token == ')':
            while stack[-1][0] != '(':
                result.append(stack.pop())
            # Discard '('.
            stack.pop()
            while stack and 'prefix' in stack[-1]:
                result.append(stack.pop())
            prefix = False
        elif prefix:
            stack.append(token + ' (prefix)')
            prefix = True
        else:
            while (stack and stack[-1] != '(' and
                   precedence[stack[-1]] >= precedence[token]):
                result.append(stack.pop())
            stack.append(token)
            prefix = True
    result.extend(stack[::-1])
    return result

tokens = read(input())
print(tokens)
instructions = convert(tokens)
print(instructions)

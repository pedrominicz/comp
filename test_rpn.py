#!/usr/bin/env python3

def read(string):
    result = []
    for word in string.split():
        try:
            result.append(int(word))
        except:
            assert len(word) == 1 and word in '+-*/^()'
            result.append(word)
    return result

precedence = {'+': 0, '-': 0, '*': 1, '/': 1, '^': 2}

def convert(infix):
    postfix = []
    stack = []
    for item in infix:
        if type(item) is int:
            postfix.append(item)
        else:
            assert type(item) is str and len(item) == 1 and item in '+-*/^()'
            if item == '(':
                stack.append(item)
            elif item == ')':
                while stack[-1] != '(':
                    postfix.append(stack.pop())
                # Discard '('
                stack.pop()
            else:
                while (stack and stack[-1] != '(' and
                       precedence[stack[-1]] > precedence[item]):
                    postfix.append(stack.pop())
                stack.append(item)
    postfix.extend(stack[::-1])
    return postfix

print(convert(read(input())))

#!/usr/bin/env python3

import random

operators = [
    '-', '+', '/', '*',
    '!=', '==', '>', '>=', '<', '<=',
]

# Keywords and identifiers
words = [
    'response', 'now', 'up', 'false', 'while', 'home', 'interest', 'hope', 'if',
    'lot', 'general', 'var', 'record', 'return', 'data', 'receive', 'my', 'fun',
    'this', 'else', 'industry', 'practice', 'network', 'apply', 'when', 'hotel',
    'cold', 'step', 'security', 'class', 'example', 'recent', 'super', 'really',
    'make', 'nil', 'agency', 'stock', 'for', 'follow', 'true', 'local', 'print',
    'and', 'end', 'or'
]

words.extend(operators)

def generate():
    if random.random() < 1 / 100:
        print()
        return
    if random.random() < 1 / 20:
        print(random.uniform(0, 10000), end=' ')
        return
    word = random.choice(words)
    if random.random() < 1 / 20:
        print(f'"{word}"', end=' ')
    else:
        print(word, end=' ')

def expression():
    return binary()

def binary():
    result = []
    for i in range(random.randint(0, 10)):
        result.extend(unary())
        result.append(random.choice(operators))
    result.extend(unary())
    return result

def unary():
    if random.random() < 1 / 3:
        result = [random.choice(["!", "-"])]
        result.extend(primary())
        return result
    return primary()

def primary():
    if random.random() < 9 / 10:
        return [str(random.randint(0, 100))]
    #if random.random() < 1 / 2:
    #    return [random.choice(['true', 'false', 'nil'])]
    #if random.random() < 1 / 2:
    #    return [f'"{random.choice(words)}"']
    result = ['(']
    result.extend(expression())
    #if random.random() < 9 / 10:
    #    result.append(')')
    result.append(')')
    return result

while True:
    #generate()
    print(' '.join(expression()))

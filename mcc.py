#!/usr/bin/env python3

import string
import sys

# Read all standard input.
input = sys.stdin.read()
input_pos = 0
input_len = len(input)

def skip_comment():
    global input_pos
    if input[input_pos:input_pos + 2] != '/*':
        return
    input_pos += 2
    while input[input_pos:input_pos + 2] != '*/':
        input_pos += 1
        assert input_pos < input_len
    # Skip '*/'.
    input_pos += 2

# Skip whitespace and comments.
def skip():
    global input_pos
    while True:
        if input_pos == input_len:
            return
        skip_comment()
        if input[input_pos] not in ' \t\n\r':
            return
        # Skip whitespace.
        input_pos += 1

# Get next token. A token is a tuple where the first entry is its type and the
# following entries the data. A token may be an integer literal, string
# literal, identifier, or operator.
def next_token():
    global input_pos
    skip()
    # End of input.
    if input_pos == input_len:
        return ()
    # Integer literal.
    if input[input_pos] in '0123456789':
        start_pos = input_pos
        while input[input_pos] in '0123456789':
            input_pos += 1
        return ('integer', int(input[start_pos:input_pos]))
    # String literal.
    if input[input_pos] == '"':
        input_pos += 1
        start_pos = input_pos
        # Throws on end of input.
        while input[input_pos] != '"':
            input_pos += 1
        string = input[start_pos:input_pos]
        input_pos += 1
        return ('string', string)
    # Character literal (results in integer literal).
    if input[input_pos] == "'":
        input_pos += 1
        # Throws on end of input.
        char = input[input_pos]
        input_pos += 1
        # Only '\n' supported.
        if char == '\\':
            char = '\n'
            assert input[input_pos] == 'n'
            input_pos += 1
        assert input[input_pos] == "'"
        input_pos += 1
        return ('integer', ord(char))
    # Identifier.
    identifier = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_'
    if input[input_pos] in identifier:
        start_pos = input_pos
        while input[input_pos] in identifier:
            input_pos += 1
        return ('identifier', input[start_pos:input_pos])
    # Operator.
    operators = ['++', '--', '&&', '||', '==', '<=', '>=', '!=', '>>', '<<']
    if input[input_pos:input_pos + 2] in operators:
        start_pos = input_pos
        input_pos += 2
        return ('operator', input[start_pos:input_pos])
    # Treat everything else as single character operators.
    operator = input[input_pos]
    input_pos += 1
    return ('operator', operator)

try:
    while token := next_token():
        sys.stdout.write(str(token) + '\n')
        sys.stdout.flush()
except:
    sys.stdout.write(str(input_pos) + '\n')
    sys.stdout.flush()

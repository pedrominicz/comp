#!/usr/bin/env python3

import sys

# Read all standard input.
input = sys.stdin.buffer.read().decode('ascii')
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

strings_len = 4096
strings = bytearray(strings_len)
strings_pos = 0

def add_string(string):
    global strings_pos
    string = string.encode('ascii')
    string_pos = strings_pos
    strings[strings_pos:strings_pos + len(string)] = string
    strings_pos += len(string)
    # Wasteful if `strings_pos % 8 == 0`.
    strings_pos = strings_pos + 8 - strings_pos % 8
    return string_pos

identifiers = ['int', 'return', 'if', 'else', 'while']

def add_identifier(identifier):
    if identifier not in identifiers:
        identifiers.append(identifier)
    return identifiers.index(identifier)

# Get next token. A token is a tuple where the first entry is its type and the
# following entries the data. A token may be an integer literal, string
# literal, identifier, or operator.
#
# Note: this function also adds string literals to `strings`.
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
        return ('string', string, add_string(string))
    # Identifier.
    identifier = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_'
    if input[input_pos] in identifier:
        start_pos = input_pos
        while input[input_pos] in identifier:
            input_pos += 1
        identifier = input[start_pos:input_pos]
        return ('identifier', identifier, add_identifier(identifier))
    # Operator.
    operators = ['==', '<=', '>=', '!=', '>>', '<<']
    if input[input_pos:input_pos + 2] in operators:
        start_pos = input_pos
        input_pos += 2
        return ('operator', input[start_pos:input_pos])
    # Treat everything else as single character operators.
    operator = input[input_pos]
    input_pos += 1
    return ('operator', operator)

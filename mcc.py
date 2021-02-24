import sys

# Read all standard input.
input = sys.stdin.read()
input_pos = 0
input_len = len(input)

# Skip comment. Expect `input_pos` to index the first character after "/*".
#
#   printf("Hello, world!"); /* Prints "Hello, world!" to the screen. */
#                              ^ input_pos
#
def skip_comment():
    global input_pos

    while input[input_pos:input_pos + 2] != '*/':
        input_pos += 1

        if input_pos == input_len:
            raise RuntimeError('Unfinished comment.')

    # Skip "*/".
    input_pos += 2

# Get next non-whitespace non-comment character.
def next_char():
    global input_pos

    while True:
        if input_pos == input_len:
            raise RuntimeError('End of input.')

        char = input[input_pos]
        input_pos += 1

        # Skip whitespace.
        if char in ' \t\n\r':
            continue

        # Skip comment.
        if char == '/' and input_pos < input_len and input[input_pos] == '*':
            input_pos + 1
            skip_comment()
            continue

        return char

try:
    while True:
        sys.stdout.write(next_char())
        sys.stdout.flush()
except RuntimeError as e:
    sys.stdout.write('\n\n')
    sys.stdout.write(str(e))
    sys.stdout.write('\n\n')

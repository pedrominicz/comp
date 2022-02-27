        .set VERSION, 1

.macro NEXT
        lodsl
        jmp *(%eax)
.endm

// Push `reg` on the return stack.
.macro PUSHRSP reg
        lea -4(%ebp), %ebp
        mov \reg, (%ebp)
.endm

// Pop top of return stack to `reg`.
.macro POPRSP reg
        mov (%ebp), \reg
        lea 4(%ebp), %ebp
.endm

        .text
        .align 4
DOCOL:
        PUSHRSP %esi
        add $4, %eax
        mov %eax, %esi
        NEXT

        .text
        .globl _start
_start:
        cld
        mov %esp, var_S0        // Save the initial data stack pointer in 
                                // `S0`.
        mov $return_stack_top, %ebp // Initialise the return stack.
        call set_up_data_segment

        mov $cold_start, %esi   // Initialize interpreter.
        NEXT                    // Run.

        .section .rodata
cold_start:
        .int QUIT

        .set F_IMMED, 0x80
        .set F_HIDDEN, 0x20
        .set F_LENMASK, 0x1f    // Length mask.

        // Store the chain of links.
        .set link, 0

.macro defword name, namelen, flags, label
        .section .rodata
        .align 4
        .globl name_\label
name_\label:
        .int link               // Link.
        .set link, name_\label
        .byte \flags + \namelen // Flags plus length byte.
        .ascii "\name"          // Name.
        .align 4                // Padding to next 4 byte boundary.
        .globl \label
\label:
        .int DOCOL              // The interpreter.
        // List of word pointers follow.
.endm

.macro defcode name, namelen, flags, label
        .section .rodata
        .align 4
        .globl name_\label
name_\label:
        .int link               // Link.
        .set link, name_\label
        .byte \flags + \namelen // Flags plus length byte.
        .ascii "\name"          // Name.
        .align 4                // Padding to next 4 byte boundary.
        .globl \label
\label:
        .int code_\label
        .text
        .globl code_\label
code_\label:
        // Assembly code follow.
.endm

        defcode "DROP", 4, 0, DROP
        pop %eax                // Drop top of stack.
        NEXT

        defcode "SWAP", 4, 0, SWAP
        pop %eax                // Swap top two elements of stack.
        pop %ebx
        push %eax
        push %ebx
        NEXT

        defcode "DUP", 3, 0, DUP
        mov (%esp), %eax        // Duplicate top of stack.
        push %eax
        NEXT

        defcode "OVER", 4, 0, OVER
        mov 4(%esp), %eax       // Get the second element of stack.
        push %eax               // Push it on top.
        NEXT

        defcode "ROT", 3, 0, ROT
        pop %eax
        pop %ebx
        pop %ecx
        push %ebx
        push %eax
        push %ecx
        NEXT

        defcode "-ROT", 4, 0, NROT
        pop %eax
        pop %ebx
        pop %ecx
        push %eax
        push %ecx
        push %ebx
        NEXT

        defcode "2DROP", 5, 0, TWODROP
        pop %eax                // Drop top two elements of stack.
        pop %eax
        NEXT

        defcode "2DUP", 4, 0, TWODUP
        mov (%esp), %eax        // Duplicate top two elements of stack.
        mov 4(%esp), %ebx
        push %ebx
        push %eax
        NEXT

        defcode "2SWAP", 5, 0, TWOSWAP
        pop %eax                // Swap top two pairs of elements of stack.
        pop %ebx
        pop %ecx
        pop %edx
        push %ebx
        push %eax
        push %edx
        push %ecx
        NEXT

        defcode "?DUP", 4, 0, QDUP
        mov (%esp), %eax        // duplicate top of stack if non-zero
        test %eax, %eax
        jz 1f
        push %eax
1:      NEXT

        defcode "1+", 2, 0, INCR
        incl (%esp)             // Increment top of stack.
        NEXT

        defcode "1-", 2, 0, DECR
        decl (%esp)             // Decrement top of stack.
        NEXT

        defcode "4+", 2, 0, INCR4
        addl $4, (%esp)         // Add 4 to top of stack.
        NEXT

        defcode "4-", 2, 0, DECR4
        subl $4, (%esp)         // Subtract 4 from top of stack.
        NEXT

        defcode "+", 1, 0, ADD
        pop %eax                // Get top of stack.
        add %eax, (%esp)        // Add it to next word on stack.
        NEXT

        defcode "-", 1, 0, SUB
        pop %eax                // Get top of stack.
        sub %eax, (%esp)        // Subtract it from next word on stack.
        NEXT

        defcode "*", 1, 0, MUL
        pop %eax
        pop %ebx
        imul %ebx, %eax
        push %eax               // Ignore overflow.
        NEXT

        defcode "/MOD", 4, 0, DIVMOD
        xor %edx, %edx
        pop %ebx
        pop %eax
        idiv %ebx
        push %edx               // push remainder
        push %eax               // push quotient
        NEXT

        defcode "=", 1, 0, EQU  // Top two words are equal?
        pop %eax
        pop %ebx
        cmp %ebx, %eax
        sete %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "<>", 2, 0, NEQU // Top two words are not equal?
        pop %eax
        pop %ebx
        cmp %ebx, %eax
        setne %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "<", 1, 0, LT
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setl %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode ">", 1, 0, GT
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setg %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "<=", 2, 0, LE
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setle %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode ">=", 2, 0, GE
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setge %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0=", 2, 0, ZEQU // Top of stack equals 0?
        pop %eax
        test %eax, %eax
        setz %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0<>", 3, 0, ZNEQU // Top of stack not 0?
        pop %eax
        test %eax, %eax
        setnz %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0<", 2, 0, ZLT // Comparisons with 0.
        pop %eax
        test %eax, %eax
        setl %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0>", 2, 0, ZGT
        pop %eax
        test %eax, %eax
        setg %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0<=", 3, 0, ZLE
        pop %eax
        test %eax, %eax
        setle %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0>=", 3, 0, ZGE
        pop %eax
        test %eax, %eax
        setge %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "AND", 3, 0, AND // Bitwise `AND`.
        pop %eax
        and %eax, (%esp)
        NEXT

        defcode "OR", 2, 0, OR  // Bitwise `OR`.
        pop %eax
        or %eax, (%esp)
        NEXT

        defcode "XOR", 3, 0, XOR // Bitwise `XOR`.
        pop %eax
        xor %eax, (%esp)
        NEXT

        defcode "INVERT", 6, 0, INVERT // Bitwise `NOT`.
        notl (%esp)
        NEXT

        defcode "EXIT", 4, 0, EXIT
        POPRSP %esi             // Pop return stack into `%esi`.
        NEXT

        defcode "LIT", 3, 0, LIT
        lodsl
        push %eax               // Push the literal number on to stack.
        NEXT

        defcode "!", 1, 0, STORE
        pop %ebx                // Address to store at.
        pop %eax                // Data to store there.
        mov %eax, (%ebx)        // Store it.
        NEXT

        defcode "@", 1, 0, FETCH
        pop %ebx                // Address to fetch.
        mov (%ebx), %eax        // Fetch it.
        push %eax               // Push value onto stack.
        NEXT

        defcode "+!", 2, 0, ADDSTORE
        pop %ebx                // Address.
        pop %eax                // The amount to add.
        add %eax, (%ebx)        // Add it.
        NEXT

        defcode "-!", 2, 0, SUBSTORE
        pop %ebx                // Address.
        pop %eax                // The amount to subtract.
        sub %eax, (%ebx)        // Add it.
        NEXT

        defcode "C!", 2, 0, STOREBYTE
        pop %ebx                // Address to store at.
        pop %eax                // Data to store there.
        mov %al, (%ebx)         // Store it.
        NEXT

        defcode "C@", 2, 0, FETCHBYTE
        pop %ebx                // Address to fetch.
        xor %eax, %eax
        mov (%ebx), %al         // Fetch it.
        push %eax               // Push value onto stack.
        NEXT

        defcode "C@C!", 4, 0, CCOPY
        mov 4(%esp), %ebx       // Source address.
        mov (%ebx), %al         // Get source character.
        pop %edi                // Destination address.
        stosb                   // Copy to destination.
        push %edi               // Increment destination address.
        incl 4(%esp)            // Increment source address.
        NEXT

        defcode "CMOVE", 5, 0, CMOVE
        mov %esi, %edx          // Preserve `%esi`.
        pop %ecx                // Length.
        pop %edi                // Destination address.
        pop %esi                // Source address.
        rep movsb               // Copy source to destination.
        mov %edx, %esi          // Restore `%esi`.
        NEXT

.macro defvar name, namelen, flags, label, initial=0
        defcode \name, \namelen, \flags, \label
        push $var_\name
        NEXT
        .data
        .align 4
var_\name:
        .int \initial
.endm

        defvar "STATE", 5, 0, STATE
        defvar "HERE", 4, 0, HERE
        defvar "LATEST", 6, 0, LATEST, name_SYSCALL0
        defvar "S0", 2, 0, SZ
        defvar "BASE", 4, 0, BASE, 10

#include <asm/unistd.h>

.macro defconst name, namelen, flags, label, value
        defcode \name, \namelen, \flags, \label
        push $\value
        NEXT
.endm

        defconst "VERSION", 7, 0, __VERSION, VERSION
        defconst "R0", 2, 0, RZ, return_stack_top
        defconst "DOCOL", 5, 0, __DOCOL, DOCOL
        defconst "F_IMMED", 7, 0, __F_IMMED, F_IMMED
        defconst "F_HIDDEN", 8, 0, __F_HIDDEN, F_HIDDEN
        defconst "F_LENMASK", 9, 0, __F_LENMASK, F_LENMASK

        defconst "SYS_EXIT", 8, 0, SYS_EXIT, __NR_exit
        defconst "SYS_OPEN", 8, 0, SYS_OPEN, __NR_open
        defconst "SYS_CLOSE", 9, 0, SYS_CLOSE, __NR_close
        defconst "SYS_READ", 8, 0, SYS_READ, __NR_read
        defconst "SYS_WRITE", 9, 0, SYS_WRITE, __NR_write
        defconst "SYS_CREAT", 9, 0, SYS_CREAT, __NR_creat
        defconst "SYS_BRK", 7, 0, SYS_BRK, __NR_brk

        defconst "O_RDONLY", 8, 0, __O_RDONLY, 0
        defconst "O_WRONLY", 8, 0, __O_WRONLY, 1
        defconst "O_RDWR", 6, 0, __O_RDWR, 2
        defconst "O_CREAT", 7, 0, __O_CREAT, 0100
        defconst "O_EXCL", 6, 0, __O_EXCL, 0200
        defconst "O_TRUNC", 7, 0, __O_TRUNC, 01000
        defconst "O_APPEND", 8, 0, __O_APPEND, 02000
        defconst "O_NONBLOCK", 10, 0, __O_NONBLOCK, 04000

        defcode ">R", 2, 0, TOR
        pop %eax                // Pop parameter stack into `%eax`.
        PUSHRSP %eax            // Push it on to the return stack.
        NEXT

        defcode "R>", 2, 0, FROMR
        POPRSP %eax             // Pop return stack on to `%eax`.
        push %eax               // And push on to parameter stack.
        NEXT

        defcode "RSP@", 4, 0, RSPFETCH
        push %ebp
        NEXT

        defcode "RSP!", 4, 0, RSPSTORE
        pop %ebp
        NEXT

        defcode "RDROP", 5, 0, RDROP
        add $4, %ebp            // Pop return stack and throw away.
        NEXT

        defcode "DSP@", 4, 0, DSPFETCH
        mov %esp, %eax
        push %eax
        NEXT

        defcode "DSP!", 4, 0, DSPSTORE
        pop %esp
        NEXT

        defcode "KEY", 3, 0, KEY
        call _KEY
        push %eax               // Push return value on stack.
        NEXT
_KEY:
        mov (currkey), %ebx
        cmp (bufftop), %ebx
        jge 1f                  // Exhausted the input buffer?
        xor %eax, %eax
        mov (%ebx), %al         // Get next key from input buffer.
        inc %ebx
        mov %ebx, (currkey)     // Increment `currkey`.
        ret
1:      // Out of input. Use `read` to fetch more input from `stdin`.
        xor %ebx, %ebx          // 1st parameter: `stdin`.
        mov $buffer, %ecx       // 2nd parameter: `buffer`.
        mov %ecx, currkey
        mov $BUFFER_SIZE, %edx  // 3rd parameter: max length.
        mov $__NR_read, %eax    // `read` system call.
        int $0x80
        test %eax, %eax         // If `%eax <= 0`, then exit.
        jbe 2f
        add %eax, %ecx          // `buffer + %eax = bufftop`.
        mov %ecx, bufftop
        jmp _KEY
2:      // Error or end of input: exit the program.
        xor %ebx, %ebx
        mov $__NR_exit, %eax    // `exit` system call.
        int $0x80

        .data
        .align 4
currkey:
        .int buffer             // Next character to read.
bufftop:
        .int buffer             // Last valid data in input `buffer + 1`.

        defcode "EMIT", 4, 0, EMIT
        pop %eax
        mov $1, %ebx            // 1st parameter: `stdout`.
        // Write needs the address of the byte to write.
        mov %al, emit_scratch
        mov $emit_scratch, %ecx // 2nd parameter: address.
        mov $1, %edx            // 3rd parameter: number of bytes.
        mov $__NR_write, %eax   // `write` system call.
        int $0x80
        NEXT

        .data
emit_scratch:
        .space 1

        defcode "WORD", 4, 0, WORD
        call _WORD
        push %edi               // Push base address.
        push %ecx               // Push length.
        NEXT
_WORD:
1:      // Search for first non-blank character.  Also skip `\` comments.
        call _KEY               // Get next key, returned in `%eax`.
        cmp $'\\', %al          // Start of a comment?
        je 3f                   // If so, skip the comment.
        cmp $' ', %al           // Is blank?
        jbe 1b                  // If so, keep looking.
        // Search for the end of the word, storing characters as we go.
        mov $word_buffer, %edi
2:      stosb                   // Add character to return buffer.
        call _KEY               // Get next key, returned in `%al`.
        cmp $' ', %al           // Is blank?
        ja 2b                   // If not, keep looping.
        // Return the word (the static buffer) and length.
        sub $word_buffer, %edi
        mov %edi, %ecx          // Return length of the word.
        mov $word_buffer, %edi  // Return address of the word.
        ret
3:      // Skip `\` comments to end of current line.
        call _KEY
        cmp $'\n', %al          // End of line?
        jne 3b
        jmp 1b

        .data
word_buffer:
        .space 32

        defcode "NUMBER", 6, 0, NUMBER
        pop %ecx                // Length of string.
        pop %edi                // Start address of string.
        call _NUMBER
        push %eax               // Parsed number.
        push %ecx               // Number of unparsed characters.
        NEXT
_NUMBER:
        xor %eax, %eax
        xor %ebx, %ebx
        test %ecx, %ecx         // Trying to parse a zero-length string is an
                                // error, but will return 0.
        jz 5f
        mov var_BASE, %edx      // Get `BASE`.
        // Check if first character is '-'.
        mov (%edi), %bl
        inc %edi
        push %eax               // Push 0 on stack.
        cmp $'-', %bl           // Negative number?
        jnz 2f
        pop %eax
        push %ebx               // Push nonzero value on stack, indicating
                                // negative.
        dec %ecx
        jnz 1f
        pop %ebx                // Error: string is only '-'.
        mov $1, %ecx
        ret
1:      // Loop reading digits.
        imul %edx, %eax         // `%eax *= BASE`.
        mov (%edi), %bl
        inc %edi
2:      // Convert `0-9`, `A-Z` to a number `0-35`.
        sub $'0', %bl
        jb 4f
        cmp $10, %bl
        jb 3f
        sub $17, %bl
        jb 4f
        add $10, %bl
3:      cmp %dl, %bl
        jge 4f
        add %ebx, %eax
        dec %ecx
        jnz 1b
4:      // Negate the result if first character was '-' (saved on the stack).
        pop %ebx
        test %ebx, %ebx
        jz 5f
        neg %eax
5:      ret

        defcode "FIND", 4, 0, FIND
        pop %ecx                // Length of string.
        pop %edi                // Start address of string.
        call _FIND
        push %eax               // Address of dictionary entry (or `NULL`).
        NEXT
_FIND:
        push %esi               // Save `%esi` so we can use it in string
                                // comparison.
        // Now we start searching backwards through the dictionary for this
        // word.
        mov var_LATEST, %edx    // `LATEST` points to name header of the
                                // latest word in the dictionary.
1:      test %edx, %edx         // `NULL` pointer?
        je 3f
        // Compare the length expected and the length of the word.
        // Note that if the `F_HIDDEN` flag is set on the word, then by a bit
        // of trickery this won't pick the word (the length will appear to be
        // wrong).
        xor %eax, %eax
        mov 4(%edx), %al        // `%al = flags + length`.
        and $(F_HIDDEN | F_LENMASK), %al
        cmp %cl, %al            // Length is the same?
        jne 2f
        // Compare the strings in detail.
        push %ecx               // Save the length.
        push %edi               // Save the address (`repe cmpsb` will move
                                // this pointer).
        lea 5(%edx), %esi       // Dictionary string we are checking against.
        repe cmpsb              // Compare the strings.
        pop %edi
        pop %ecx
        jne 2f                  // Not the same.
        pop %esi
        mov %edx, %eax
        ret
2:      mov (%edx), %edx        // Move back through the link field to
                                // previous word.
        jmp 1b                  // Loop.
3:      // Not found.
        pop %esi
        xor %eax, %eax          // Return zero to indicate not found.
        ret

        defcode ">CFA", 4, 0, TCFA
        pop %edi
        call _TCFA
        push %edi
        NEXT
_TCFA:
        xor %eax, %eax
        add $4, %edi            // Skip link pointer.
        mov (%edi), %al         // Load `flags + len` into `%al`.
        inc %edi                // Skip `flags + len` byte.
        and $F_LENMASK, %al     // Just the length, not the flags.
        add %eax, %edi          // Skip the name.
        add $3, %edi            // The codeword is 4-byte aligned.
        and $~3, %edi
        ret

        defword ">DFA", 4, 0, TDFA
        .int TCFA
        .int INCR4
        .int EXIT

        defcode "CREATE", 6, 0, CREATE
        // Get the name length and address.
        pop %ecx                // Length of string.
        pop %ebx                // Start address of string.
        // Link pointer.
        mov var_HERE, %edi      // `%edi` is the address of the header.
        mov var_LATEST, %eax    // Get link pointer.
        stosl                   // Store it in the header.
        // Length byte and the word itself.
        mov %cl, %al            // Get length.
        stosb                   // Store the length/flags byte.
        push %esi
        mov %ebx, %esi          // `%esi = word`.
        rep movsb               // Copy the word.
        pop %esi
        add $3, %edi            // Align to next 4-byte boundary.
        and $~3, %edi
        // Update `LATEST` and `HERE`.
        mov var_HERE, %eax
        mov %eax, var_LATEST
        mov %edi, var_HERE
        NEXT

        defcode ",", 1, 0, COMMA
        pop %eax                // Code pointer to store.
        call _COMMA
        NEXT
_COMMA:
        mov var_HERE, %edi      // `HERE`.
        stosl                   // Store it.
        mov %edi, var_HERE      // Update `HERE`.
        ret

        defcode "[", 1, F_IMMED, LBRAC
        xor %eax, %eax
        mov %eax, var_STATE     // Set `STATE` to immediate mode.
        NEXT

        defcode "]", 1, 0, RBRAC
        movl $1, var_STATE      // Set `STATE` to compile mode.
        NEXT

        defword ":", 1, 0, COLON
        .int WORD               // Get the name of the new word.
        .int CREATE             // Create the dictionary entry/header.
        .int LIT, DOCOL, COMMA  // Append `DOCOL` codeword.
        .int LATEST, FETCH, HIDDEN // Make the word hidden.
        .int RBRAC              // Go into compile mode.
        .int EXIT

        defword ";", 1, F_IMMED, SEMICOLON
        .int LIT, EXIT, COMMA   // Append `EXIT`.
        .int LATEST, FETCH, HIDDEN // Toggle hidden flag (unhide the word).
        .int LBRAC              // Go into immediate mode
        .int EXIT

        defcode "IMMEDIATE", 9, F_IMMED, IMMEDIATE
        mov var_LATEST, %edi    // `LATEST` word.
        add $4, %edi            // Point to the name/flags byte.
        xorb $F_IMMED, (%edi)   // Toggle `IMMED` bit.
        NEXT

        defcode "HIDDEN", 6, 0, HIDDEN
        pop %edi                // Dictionary entry.
        add $4, %edi            // Point to name/flags byte.
        xorb $F_HIDDEN, (%edi)  // Toggle `HIDDEN` bit.
        NEXT

        defword "HIDE", 4, 0, HIDE
        .int WORD               // Get the word after `HIDE`.
        .int FIND               // Look up in the dictionary.
        .int HIDDEN             // Set `F_HIDDEN` flag.
        .int EXIT

        defcode "'", 1, 0, TICK
        lodsl                   // Get the address of the next word and skip
                                // it.
        push %eax               // Push it on the stack.
        NEXT

        defcode "BRANCH", 6, 0, BRANCH
        add (%esi), %esi        // Add the offset to the instruction pointer.
        NEXT

        defcode "0BRANCH", 7, 0, ZBRANCH
        pop %eax
        test %eax, %eax         // Top of stack is zero?
        jz code_BRANCH          // If so, jump to the branch function above.
        lodsl                   // Otherwise skip the offset.
        NEXT

        defcode "LITSTRING", 9, 0, LITSTRING
        lodsl                   // Get length of string.
        push %esi               // Push address of start of string.
        push %eax               // Push it on the stack.
        add %eax, %esi          // Skip past string.
        add $3, %esi            // Round up to next 4-byte boundary.
        and $~3, %esi
        NEXT

        defcode "TELL", 4, 0, TELL
        mov $1, %ebx            // 1st parameter: `stdout`.
        pop %edx                // 3rd parameter: length of string.
        pop %ecx                // 2nd parameter: address of string.
        mov $__NR_write, %eax   // `write` system call.
        int $0x80
        NEXT

        defword "QUIT", 4, 0, QUIT
        .int RZ, RSPSTORE       // Clear the return stack.
        .int INTERPRET          // Interpret the next word.
        .int BRANCH, -8         // Loop.

        defcode "INTERPRET", 9, 0, INTERPRET
        call _WORD              // Returns `%ecx = length` and `%edi = pointer
                                // to word`.
        // Is it in the dictionary?
        xor %eax, %eax
        mov %eax, interpret_is_lit // Not (yet) a number literal.
        call _FIND              // Return `%eax = pointer to header` or `%eax
                                // = NULL` if not found.
        test %eax, %eax         // Found?
        jz 1f
        // In the dictionary. Is it an immediate codeword?
        mov %eax, %edi          // `%edi = dictionary entry`.
        mov 4(%edi), %al        // Get length/flags.
        push %ax
        call _TCFA              // Convert dictionary entry (in `%edi`) to
                                // codeword pointer.
        pop %ax
        and $F_IMMED, %al       // Is `IMMED` flag set?
        mov %edi, %eax
        jnz 4f                  // If `IMMED`, jump straight to executing.
        jmp 2f

1:      // Not in dictionary, assume it is a number literal.
        incl interpret_is_lit
        call _NUMBER            // Returns parsed number in `%eax`, `%ecx > 0`
                                // if error.
        test %ecx, %ecx
        jnz 6f
        mov %eax, %ebx
        mov $LIT, %eax          // The word is `LIT`.

2:      // Are we compiling or executing?
        mov var_STATE, %edx
        test %edx, %edx
        jz 4f                   // Jump if executing.
        // Compiling: just append the word to the current dictionary
        // definition.
        call _COMMA             // Stores `%eax` at `HERE` and increments
                                // `HERE`.
        mov interpret_is_lit, %ecx // Was it a literal?
        test %ecx, %ecx
        jz 3f
        mov %ebx, %eax          // Yes, so `LIT` is followed by a number.
        call _COMMA

3:      NEXT

4:      // Executing.
        mov interpret_is_lit, %ecx // Was it a literal?
        test %ecx, %ecx
        jnz 5f
        // Not a literal, execute it now. This never returns, but the codeword
        // will eventually call `NEXT` which will reenter the loop in `QUIT`.
        jmp *(%eax)

5:      // Executing a literal: push it on the stack.
        push %ebx
        NEXT

6:      // Parse error (not a known word or a number in current `BASE`). Print
        // an error message followed by up to 40 characters of context.
        mov $2, %ebx            // 1st parameter: `stderr`.
        mov $errmsg, %ecx       // 2nd parameter: error message.
        mov $errmsgend - errmsg, %edx // 3rd parameter: length of string.
        mov $__NR_write, %eax   // `write` system call.
        int $0x80
        mov (currkey), %ecx     // The error occurred just before `currkey`.
        mov %ecx, %edx
        sub $buffer, %edx       // `%edx = currkey - buffer`.
        cmp $40, %edx           // If greater than 40, then print only 40
                                // characters.
        jle 7f
        mov $40, %edx

7:      sub %edx, %ecx          // `%ecx = start of area to print`, `%edx =
                                // length`.
        mov $__NR_write, %eax   // `write` system call.
        int $0x80
        mov $errmsgnl, %ecx     // Newline.
        mov $1, %edx
        mov $__NR_write, %eax   // `write` system call.
        int $0x80
        NEXT

        .section .rodata
errmsg: .ascii "PARSE ERROR: "
errmsgend:
errmsgnl: .ascii "\n"

        .data
        .align 4
interpret_is_lit:
        .int 0                  // Flag used to record if reading a literal.

        defcode "CHAR", 4, 0, CHAR
        call _WORD              // Returns `%ecx = length`, `%edi = pointer to
                                // word`.
        xor %eax, %eax
        mov (%edi), %al         // Get first character of word.
        push %eax               // Push it onto the stack.
        NEXT

        defcode "EXECUTE", 7, 0, EXECUTE
        pop %eax
        jmp *(%eax)

        defcode "SYSCALL3", 8, 0, SYSCALL3
        pop %eax                // System call number.
        pop %ebx                // First parameter.
        pop %ecx                // Second parameter.
        pop %edx                // Third parameter.
        int $0x80
        push %eax               // Result.
        NEXT

        defcode "SYSCALL2", 8, 0, SYSCALL2
        pop %eax                // System call number.
        pop %ebx                // First parameter.
        pop %ecx                // Second parameter.
        int $0x80
        push %eax               // Result.
        NEXT

        defcode "SYSCALL1", 8, 0, SYSCALL1
        pop %eax                // System call number.
        pop %ebx                // First parameter.
        int $0x80
        push %eax               // Result.
        NEXT

        defcode "SYSCALL0", 8, 0, SYSCALL0
        pop %eax                // System call number.
        int $0x80
        push %eax               // Result.
        NEXT

        .text
        .set INITIAL_DATA_SEGMENT_SIZE, 65536
set_up_data_segment:
        xor %ebx, %ebx          // Call `brk(0)`.
        mov $__NR_brk, %eax
        int $0x80
        mov %eax, var_HERE      // Initialise `HERE` to point at beginning of
                                // data segment.
        add $INITIAL_DATA_SEGMENT_SIZE, %eax
        mov %eax, %ebx
        mov $__NR_brk, %eax
        int $0x80
        ret

        .set RETURN_STACK_SIZE, 8192
        .set BUFFER_SIZE, 4096

        .bss
        .align 4096
return_stack:
        .space RETURN_STACK_SIZE
return_stack_top:               // Initial top of return stack.

        .align 4096
buffer:
        .space BUFFER_SIZE

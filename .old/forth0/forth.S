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
docol:
        PUSHRSP %esi
        add $4, %eax
        mov %eax, %esi
        NEXT

        .text
        .globl _start
_start:
        cld
        mov %esp, var_s0        // Save the initial data stack pointer in
                                // `s0`.
        mov $return_stack_top, %ebp // Initialise the return stack.
        call set_up_data_segment

        mov $cold_start, %esi   // Initialize interpreter.
        NEXT                    // Run.

        .section .rodata
cold_start:
        .int quit

        .set F_IMMED, 0x80
        .set F_HIDDEN, 0x20
        .set F_LENMASK, 0x1f    // Length mask.

        // Store the chain of links.
        .set link, 0

.macro defword name, namelen, label, flags=0
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
        .int docol              // The interpreter.
        // List of word pointers follow.
.endm

.macro defcode name, namelen, label, flags=0
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

        defcode "drop", 4, drop
        pop %eax                // Drop top of stack.
        NEXT

        defcode "swap", 4, swap
        pop %eax                // Swap top two elements of stack.
        pop %ebx
        push %eax
        push %ebx
        NEXT

        defcode "dup", 3, dup
        mov (%esp), %eax        // Duplicate top of stack.
        push %eax
        NEXT

        defcode "over", 4, over
        mov 4(%esp), %eax       // Get the second element of stack.
        push %eax               // Push it on top.
        NEXT

        defcode "rot", 3, rot
        pop %eax
        pop %ebx
        pop %ecx
        push %ebx
        push %eax
        push %ecx
        NEXT

        defcode "-rot", 4, nrot
        pop %eax
        pop %ebx
        pop %ecx
        push %eax
        push %ecx
        push %ebx
        NEXT

        defcode "2drop", 5, twodrop
        pop %eax                // Drop top two elements of stack.
        pop %eax
        NEXT

        defcode "2dup", 4, twodup
        mov (%esp), %eax        // Duplicate top two elements of stack.
        mov 4(%esp), %ebx
        push %ebx
        push %eax
        NEXT

        defcode "2swap", 5, twoswap
        pop %eax                // Swap top two pairs of elements of stack.
        pop %ebx
        pop %ecx
        pop %edx
        push %ebx
        push %eax
        push %edx
        push %ecx
        NEXT

        defcode "?dup", 4, qdup
        mov (%esp), %eax        // Duplicate top of stack if non-zero.
        test %eax, %eax
        jz 1f
        push %eax
1:      NEXT

        defcode "1+", 2, incr
        incl (%esp)             // Increment top of stack.
        NEXT

        defcode "1-", 2, decr
        decl (%esp)             // Decrement top of stack.
        NEXT

        defcode "4+", 2, incr4
        addl $4, (%esp)         // Add 4 to top of stack.
        NEXT

        defcode "4-", 2, decr4
        subl $4, (%esp)         // Subtract 4 from top of stack.
        NEXT

        defcode "+", 1, add
        pop %eax                // Get top of stack.
        add %eax, (%esp)        // Add it to next word on stack.
        NEXT

        defcode "-", 1, sub
        pop %eax                // Get top of stack.
        sub %eax, (%esp)        // Subtract it from next word on stack.
        NEXT

        defcode "*", 1, mul
        pop %eax
        pop %ebx
        imul %ebx, %eax
        push %eax               // Ignore overflow.
        NEXT

        defcode "/mod", 4, divmod
        xor %edx, %edx
        pop %ebx
        pop %eax
        idiv %ebx
        push %edx               // push remainder
        push %eax               // push quotient
        NEXT

        defcode "=", 1, equ     // Top two words are equal?
        pop %eax
        pop %ebx
        cmp %ebx, %eax
        sete %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "<>", 2, nequ   // Top two words are not equal?
        pop %eax
        pop %ebx
        cmp %ebx, %eax
        setne %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "<", 1, lt
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setl %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode ">", 1, gt
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setg %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "<=", 2, le
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setle %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode ">=", 2, ge
        pop %eax
        pop %ebx
        cmp %eax, %ebx
        setge %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0=", 2, zequ   // Top of stack equals 0?
        pop %eax
        test %eax, %eax
        setz %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0<>", 3, znequ // Top of stack not 0?
        pop %eax
        test %eax, %eax
        setnz %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0<", 2, zlt    // Comparisons with 0.
        pop %eax
        test %eax, %eax
        setl %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0>", 2, zgt
        pop %eax
        test %eax, %eax
        setg %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0<=", 3, zle
        pop %eax
        test %eax, %eax
        setle %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "0>=", 3, zge
        pop %eax
        test %eax, %eax
        setge %al
        movzb %al, %eax
        push %eax
        NEXT

        defcode "and", 3, and   // Bitwise `and`.
        pop %eax
        and %eax, (%esp)
        NEXT

        defcode "or", 2, or     // Bitwise `or`.
        pop %eax
        or %eax, (%esp)
        NEXT

        defcode "xor", 3, xor   // Bitwise `xor`.
        pop %eax
        xor %eax, (%esp)
        NEXT

        defcode "invert", 6, invert // Bitwise `not`.
        notl (%esp)
        NEXT

        defcode "lshift", 6, lshift
        pop %ecx
        shll %cl, (%esp)
        NEXT

        defcode "rshift", 6, rshift
        pop %ecx
        shrl %cl, (%esp)
        NEXT

        defcode "exit", 4, exit
        POPRSP %esi             // Pop return stack into `%esi`.
        NEXT

        defcode "lit", 3, lit
        lodsl
        push %eax               // Push the literal number on to stack.
        NEXT

        defcode "!", 1, store
        pop %ebx                // Address to store at.
        pop %eax                // Data to store there.
        mov %eax, (%ebx)        // Store it.
        NEXT

        defcode "@", 1, fetch
        pop %ebx                // Address to fetch.
        mov (%ebx), %eax        // Fetch it.
        push %eax               // Push value onto stack.
        NEXT

        defcode "+!", 2, addstore
        pop %ebx                // Address.
        pop %eax                // The amount to add.
        add %eax, (%ebx)        // Add it.
        NEXT

        defcode "-!", 2, substore
        pop %ebx                // Address.
        pop %eax                // The amount to subtract.
        sub %eax, (%ebx)        // Add it.
        NEXT

        defcode "c!", 2, storebyte
        pop %ebx                // Address to store at.
        pop %eax                // Data to store there.
        mov %al, (%ebx)         // Store it.
        NEXT

        defcode "c@", 2, fetchbyte
        pop %ebx                // Address to fetch.
        xor %eax, %eax
        mov (%ebx), %al         // Fetch it.
        push %eax               // Push value onto stack.
        NEXT

        defcode "c@c!", 4, ccopy
        mov 4(%esp), %ebx       // Source address.
        mov (%ebx), %al         // Get source character.
        pop %edi                // Destination address.
        stosb                   // Copy to destination.
        push %edi               // Increment destination address.
        incl 4(%esp)            // Increment source address.
        NEXT

        defcode "cmove", 5, cmove
        mov %esi, %edx          // Preserve `%esi`.
        pop %ecx                // Length.
        pop %edi                // Destination address.
        pop %esi                // Source address.
        rep movsb               // Copy source to destination.
        mov %edx, %esi          // Restore `%esi`.
        NEXT

.macro defvar name, namelen, label, initial=0, flags=0
        defcode \name, \namelen, \label, \flags
        push $var_\name
        NEXT
        .data
        .align 4
var_\name:
        .int \initial
.endm

        defvar "state", 5, state
        defvar "here", 4, here
        defvar "latest", 6, latest, name_syscall0
        defvar "s0", 2, sz
        defvar "base", 4, base, 10

#include <asm/unistd.h>

.macro defconst name, namelen, label, value, flags=0
        defcode \name, \namelen, \label, \flags
        push $\value
        NEXT
.endm

        defconst "version", 7, __version, VERSION
        defconst "r0", 2, rz, return_stack_top
        defconst "docol", 5, __docol, docol
        defconst "f_immed", 7, __f_immed, F_IMMED
        defconst "f_hidden", 8, __f_hidden, F_HIDDEN
        defconst "f_lenmask", 9, __f_lenmask, F_LENMASK

        defconst "sys_exit", 8, sys_exit, __NR_exit
        defconst "sys_open", 8, sys_open, __NR_open
        defconst "sys_close", 9, sys_close, __NR_close
        defconst "sys_read", 8, sys_read, __NR_read
        defconst "sys_write", 9, sys_write, __NR_write
        defconst "sys_creat", 9, sys_creat, __NR_creat
        defconst "sys_brk", 7, sys_brk, __NR_brk

        defconst "o_rdonly", 8, __o_rdonly, 0
        defconst "o_wronly", 8, __o_wronly, 1
        defconst "o_rdwr", 6, __o_rdwr, 2
        defconst "o_creat", 7, __o_creat, 0100
        defconst "o_excl", 6, __o_excl, 0200
        defconst "o_trunc", 7, __o_trunc, 01000
        defconst "o_append", 8, __o_append, 02000
        defconst "o_nonblock", 10, __o_nonblock, 04000

        defcode ">r", 2, tor
        pop %eax                // Pop parameter stack into `%eax`.
        PUSHRSP %eax            // Push it on to the return stack.
        NEXT

        defcode "r>", 2, fromr
        POPRSP %eax             // Pop return stack on to `%eax`.
        push %eax               // And push on to parameter stack.
        NEXT

        defcode "rsp@", 4, rspfetch
        push %ebp
        NEXT

        defcode "rsp!", 4, rspstore
        pop %ebp
        NEXT

        defcode "rdrop", 5, rdrop
        add $4, %ebp            // Pop return stack and throw away.
        NEXT

        defcode "dsp@", 4, dspfetch
        mov %esp, %eax
        push %eax
        NEXT

        defcode "dsp!", 4, dspstore
        pop %esp
        NEXT

        defcode "key", 3, key
        call _key
        push %eax               // Push return value on stack.
        NEXT
_key:
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
        jmp _key
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

        defcode "emit", 4, emit
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

        defcode "word", 4, word
        call _word
        push %edi               // Push base address.
        push %ecx               // Push length.
        NEXT
_word:
1:      // Search for first non-blank character.  Also skip `\` comments.
        call _key               // Get next key, returned in `%eax`.
        cmp $'\\', %al          // Start of a comment?
        je 3f                   // If so, skip the comment.
        cmp $' ', %al           // Is blank?
        jbe 1b                  // If so, keep looking.
        // Search for the end of the word, storing characters as we go.
        mov $word_buffer, %edi
2:      stosb                   // Add character to return buffer.
        call _key               // Get next key, returned in `%al`.
        cmp $' ', %al           // Is blank?
        ja 2b                   // If not, keep looping.
        // Return the word (the static buffer) and length.
        sub $word_buffer, %edi
        mov %edi, %ecx          // Return length of the word.
        mov $word_buffer, %edi  // Return address of the word.
        ret
3:      // Skip `\` comments to end of current line.
        call _key
        cmp $'\n', %al          // End of line?
        jne 3b
        jmp 1b

        .data
word_buffer:
        .space 32

        defcode "number", 6, number
        pop %ecx                // Length of string.
        pop %edi                // Start address of string.
        call _number
        push %eax               // Parsed number.
        push %ecx               // Number of unparsed characters.
        NEXT
_number:
        xor %eax, %eax
        xor %ebx, %ebx
        test %ecx, %ecx         // Trying to parse a zero-length string is an
                                // error, but will return 0.
        jz 5f
        mov var_base, %edx      // Get `base`.
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
        imul %edx, %eax         // `%eax *= base`.
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

        defcode "find", 4, find
        pop %ecx                // Length of string.
        pop %edi                // Start address of string.
        call _find
        push %eax               // Address of dictionary entry (or `NULL`).
        NEXT
_find:
        push %esi               // Save `%esi` so we can use it in string
                                // comparison.
        // Now we start searching backwards through the dictionary for this
        // word.
        mov var_latest, %edx    // `latest` points to name header of the
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

        defcode ">cfa", 4, tcfa
        pop %edi
        call _tcfa
        push %edi
        NEXT
_tcfa:
        xor %eax, %eax
        add $4, %edi            // Skip link pointer.
        mov (%edi), %al         // Load `flags + len` into `%al`.
        inc %edi                // Skip `flags + len` byte.
        and $F_LENMASK, %al     // Just the length, not the flags.
        add %eax, %edi          // Skip the name.
        add $3, %edi            // The codeword is 4-byte aligned.
        and $~3, %edi
        ret

        defword ">dfa", 4, tdfa
        .int tcfa
        .int incr4
        .int exit

        defcode "create", 6, create
        // Get the name length and address.
        pop %ecx                // Length of string.
        pop %ebx                // Start address of string.
        // Link pointer.
        mov var_here, %edi      // `%edi` is the address of the header.
        mov var_latest, %eax    // Get link pointer.
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
        // Update `latest` and `here`.
        mov var_here, %eax
        mov %eax, var_latest
        mov %edi, var_here
        NEXT

        defcode ",", 1, comma
        pop %eax                // Code pointer to store.
        call _comma
        NEXT
_comma:
        mov var_here, %edi      // `here`.
        stosl                   // Store it.
        mov %edi, var_here      // Update `here`.
        ret

        defcode "[", 1, lbrac, F_IMMED
        xor %eax, %eax
        mov %eax, var_state     // Set `state` to immediate mode.
        NEXT

        defcode "]", 1, rbrac
        movl $1, var_state      // Set `state` to compile mode.
        NEXT

        defword ":", 1, colon
        .int word               // Get the name of the new word.
        .int create             // Create the dictionary entry/header.
        .int lit, docol, comma  // Append `docol` codeword.
        .int latest, fetch, hidden // Make the word hidden.
        .int rbrac              // Go into compile mode.
        .int exit

        defword ";", 1, semicolon, F_IMMED
        .int lit, exit, comma   // Append `exit`.
        .int latest, fetch, hidden // Toggle hidden flag (unhide the word).
        .int lbrac              // Go into immediate mode
        .int exit

        defcode "immediate", 9, immediate, F_IMMED
        mov var_latest, %edi    // `latest` word.
        add $4, %edi            // Point to the name/flags byte.
        xorb $F_IMMED, (%edi)   // Toggle "immediate" bit.
        NEXT

        defcode "hidden", 6, hidden
        pop %edi                // Dictionary entry.
        add $4, %edi            // Point to name/flags byte.
        xorb $F_HIDDEN, (%edi)  // Toggle "hidden" bit.
        NEXT

        defword "hide", 4, hide
        .int word               // Get the word after `hide`.
        .int find               // Look up in the dictionary.
        .int hidden             // Set `F_HIDDEN` flag.
        .int exit

        defcode "'", 1, tick
        lodsl                   // Get the address of the next word and skip
                                // it.
        push %eax               // Push it on the stack.
        NEXT

        defcode "branch", 6, branch
        add (%esi), %esi        // Add the offset to the instruction pointer.
        NEXT

        defcode "0branch", 7, zbranch
        pop %eax
        test %eax, %eax         // Top of stack is zero?
        jz code_branch          // If so, jump to the branch function above.
        lodsl                   // Otherwise skip the offset.
        NEXT

        defcode "litstring", 9, litstring
        lodsl                   // Get length of string.
        push %esi               // Push address of start of string.
        push %eax               // Push it on the stack.
        add %eax, %esi          // Skip past string.
        add $3, %esi            // Round up to next 4-byte boundary.
        and $~3, %esi
        NEXT

        defcode "tell", 4, tell
        mov $1, %ebx            // 1st parameter: `stdout`.
        pop %edx                // 3rd parameter: length of string.
        pop %ecx                // 2nd parameter: address of string.
        mov $__NR_write, %eax   // `write` system call.
        int $0x80
        NEXT

        defword "quit", 4, quit
        .int rz, rspstore       // Clear the return stack.
        .int interpret          // Interpret the next word.
        .int branch, -8         // Loop.

        defcode "interpret", 9, interpret
        call _word              // Returns `%ecx = length` and `%edi = pointer
                                // to word`.
        // Is it in the dictionary?
        xor %eax, %eax
        mov %eax, interpret_is_lit // Not (yet) a number literal.
        call _find              // Return `%eax = pointer to header` or `%eax
                                // = NULL` if not found.
        test %eax, %eax         // Found?
        jz 1f
        // In the dictionary. Is it an immediate codeword?
        mov %eax, %edi          // `%edi = dictionary entry`.
        mov 4(%edi), %al        // Get length/flags.
        push %ax
        call _tcfa              // Convert dictionary entry (in `%edi`) to
                                // codeword pointer.
        pop %ax
        and $F_IMMED, %al       // Is "immediate" flag set?
        mov %edi, %eax
        jnz 4f                  // If "immediate", jump straight to executing.
        jmp 2f

1:      // Not in dictionary, assume it is a number literal.
        incl interpret_is_lit
        call _number            // Returns parsed number in `%eax`, `%ecx > 0`
                                // if error.
        test %ecx, %ecx
        jnz 6f
        mov %eax, %ebx
        mov $lit, %eax          // The word is `lit`.

2:      // Are we compiling or executing?
        mov var_state, %edx
        test %edx, %edx
        jz 4f                   // Jump if executing.
        // Compiling: just append the word to the current dictionary
        // definition.
        call _comma             // Stores `%eax` at `here` and increments
                                // `here`.
        mov interpret_is_lit, %ecx // Was it a literal?
        test %ecx, %ecx
        jz 3f
        mov %ebx, %eax          // Yes, so `lit` is followed by a number.
        call _comma

3:      NEXT

4:      // Executing.
        mov interpret_is_lit, %ecx // Was it a literal?
        test %ecx, %ecx
        jnz 5f
        // Not a literal, execute it now. This never returns, but the codeword
        // will eventually call `NEXT` which will reenter the loop in `quit`.
        jmp *(%eax)

5:      // Executing a literal: push it on the stack.
        push %ebx
        NEXT

6:      // Parse error (not a known word or a number in current `base`). Print
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
errmsg: .ascii "parse error: "
errmsgend:
errmsgnl: .ascii "\n"

        .data
        .align 4
interpret_is_lit:
        .int 0                  // Flag used to record if reading a literal.

        defcode "char", 4, char
        call _word              // Returns `%ecx = length`, `%edi = pointer to
                                // word`.
        xor %eax, %eax
        mov (%edi), %al         // Get first character of word.
        push %eax               // Push it onto the stack.
        NEXT

        defcode "execute", 7, execute
        pop %eax
        jmp *(%eax)

        defcode "syscall3", 8, syscall3
        pop %eax                // System call number.
        pop %ebx                // First parameter.
        pop %ecx                // Second parameter.
        pop %edx                // Third parameter.
        int $0x80
        push %eax               // Result.
        NEXT

        defcode "syscall2", 8, syscall2
        pop %eax                // System call number.
        pop %ebx                // First parameter.
        pop %ecx                // Second parameter.
        int $0x80
        push %eax               // Result.
        NEXT

        defcode "syscall1", 8, syscall1
        pop %eax                // System call number.
        pop %ebx                // First parameter.
        int $0x80
        push %eax               // Result.
        NEXT

        defcode "syscall0", 8, syscall0
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
        mov %eax, var_here      // Initialise `here` to point at beginning of
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

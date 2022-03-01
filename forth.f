: / /mod swap drop ;
: mod /mod drop ;

: '\n' 10 ;
: bl 32 ;

: cr '\n' emit ;
: space bl emit ;

: negate 0 swap - ;

: true 1 ;
: false 0 ;
: not 0= ;

: literal immediate ' lit , , ;

: ':' [ char : ] literal ;
: ';' [ char ; ] literal ;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'A' [ char A ] literal ;
: '0' [ char 0 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;

: [compile] immediate word find >cfa , ;

: recurse immediate latest @ >cfa , ;

\ Usage: `CONDITION if TRUE-PART then`.
: if immediate
        ' 0branch ,     \ Compile `0branch`.
        here @          \ Save location of the offset on stack.
        0 ,             \ Compile dummy offset.
;

: then immediate
        dup
        here @ swap -   \ Calculate the offset from address saved on stack.
        swap !          \ Store offset.
;

\ Usage: `CONDITION if TRUE-PART else FALSE-PART then`.
: else immediate
        ' branch ,      \ Compile `branch` over the false part.
        here @          \ Save location of the offset on stack.
        0 ,             \ Compile dummy offset.
        swap
        dup
        here @ swap -   \ Calculate the offset from address saved on stack.
        swap !          \ Store offset.
;

\ Usage: `begin LOOP-PART CONDITION until`.
\ C equivalent: `do { LOOP-PART } while (CONDITION)`.
: begin immediate
        here @          \ Save location on stack.
;

: until immediate
        ' 0branch ,     \ Compile `0branch`.
        here @ -        \ Calculate offset from address saved on stack.
        ,               \ Compile offset.
;

\ Usage: `begin LOOP-PART again`.
\ C equivalent: `do { LOOP-PART } while (1)`.
: again immediate
        ' branch ,      \ Compile `branch`.
        here @ -        \ Calculate offset from address saved on stack.
        ,               \ Compile offset.
;

\ Usage: `begin CONDITION while LOOP-PART repeat`.
: while immediate
        ' 0branch ,     \ Compile `0branch`.
        here @          \ Save location on stack.
        0 ,             \ Compile dummy offset.
;

: repeat immediate
        ' branch ,      \ Compile `branch`.
        swap            \ Get offset (from `begin`).
        here @ - ,      \ Calculate and compile offset.
        dup
        here @ swap -   \ Calculate offset (from `while`).
        swap !          \ Store offset.
;

\ TODO: verify if usage below is correct.
\ Usage: `CONDITION unless FALSE-PART then`.
\ Usage: `CONDITION unless FALSE-PART else TRUE-PART then`.
: unless immediate
        ' not ,         \ Compile `not`.
        [compile] if    \ Call `if`.
;

\ Comments.
: ( immediate
        1               \ Allow nested comments (keep track of depth).
        begin
                key     \ Read next character.
                dup '(' = if \ Increase depth if character equals '('.
                        drop
                        1+
                else
                        ')' = if \ Decrease depth if character equals ')'.
                                1-
                        then
                then
        dup 0= until    \ Continue until depth equals zero.
        drop            \ Drop depth counter.
;

: nip ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
        1+              \ Add one to skip over `u`.
        4 *             \ Multiply by word size.
        dsp@ +          \ Add stack pointer.
        @               \ Fetch.
;

: spaces ( n -- )
        begin
                dup 0>  \ While `n > 0`.
        while
                space   \ Print a space.
                1-      \ Count down to zero.
        repeat
        drop
;

: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;

\ Print unsigned number.
: u. ( u -- )
        base @ /mod
        ?dup if
                recurse
        then

        dup 10 < if
                '0'
        else
                10 -
                'A'
        then
        +
        emit
;

\ Print stack.
: .s ( -- )
        dsp@
        begin
                dup s0 @ <
        while
                dup @ u.
                space
                4+
        repeat
        drop
;

\ Return width (in characters) of unsigned number in current base.
: uwidth ( u -- width )
        base @ /
        ?dup if
                recurse 1+
        else
                1
        then
;

: u.r ( u width -- )
        swap
        dup
        uwidth
        rot
        swap -
        spaces          \ It is safe to call spaces on a negative number.
        u.
;

: .r ( n width -- )
        swap
        dup 0< if
                negate
                1
                swap
                rot
                1-
        else
                0
                swap
                rot
        then
        swap
        dup
        uwidth
        rot
        swap -

        spaces
        swap

        if
                '-' emit
        then

        u.
;

: . 0 .r space ;

: u. u. space ;

: ? ( addr -- ) @ . ;

\ Returns `a <= c && c < b`.
: within ( c a b -- bool )
        -rot
        over
        <= if
                > if
                        true
                else
                        false
                then
        else
                2drop
                false
        then
;

: depth ( -- n )
        s0 @ dsp@ -
        4-
;

: aligned ( addr -- addr )
        3 + 3 invert and
;

: align here @ aligned here ! ;

: c, ( char -- )
        here @ c!       \ Store the character in `here`.
        1 here +!       \ Increment `here` by one.
;

: s" immediate ( -- addr len )
        state @ if      \ Compile mode.
                ' litstring ,
                here @
                0 ,
                begin
                        key
                        dup '"' <>
                while
                        c,
                repeat
                drop
                dup
                here @ swap -
                4-
                swap !
                align
        else            \ Immediate mode.
                here @
                begin
                        key
                        dup '"' <>
                while
                        over c!
                        1+
                repeat
                drop
                here @ -
                here @
                swap
        then
;

: ." immediate ( -- )
        state @ if      \ Compile mode.
                [compile] s"
                ' tell ,
        else            \ Immediate mode.
                begin
                        key
                        dup '"' = if
                                drop
                                exit
                        then
                        emit
                again
        then
;

\ Usage: `VALUE constant NAME`.
: constant
        word create
        docol ,
        ' lit ,
        ,
        ' exit ,
;

: allot ( n -- addr )
        here @ swap
        here +!
;

: cells ( n -- n ) 4 * ;

\ Usage: `variable NAME`.
: variable
        1 cells allot
        word create
        docol ,
        ' lit ,
        ,
        ' exit ,
;

\ Usage: `VALUE value NAME`.
: value ( n -- )
        [compile] constant
;

: to immediate ( n -- )
        word
        find
        >dfa
        4+
        state @ if      \ Compile mode.
                ' lit ,
                ,
                ' ! ,
        else            \ Immediate mode.
                !
        then
;

: +to immediate ( n -- )
        word
        find
        >dfa
        4+
        state @ if      \ Compile mode.
                ' lit ,
                ,
                ' +! ,
        else            \ Immediate mode.
                +!
        then
;

: id.
        4+
        dup c@
        f_lenmask and

        begin
                dup 0>
        while
                swap 1+
                dup c@
                emit
                swap 1-
        repeat
        2drop
;

: ?hidden
        4+
        c@
        f_hidden and
;

: ?immediate
        4+
        c@
        f_immed and
;

: words
        latest @
        begin
                ?dup
        while
                dup ?hidden not if
                        dup id.
                        space
                then
                @
        repeat
        cr
;

: forget
        word find
        dup @ latest !
        here !
;

: dump ( addr len -- )
        base @ -rot
        hex

        begin
                ?dup
        while
                over 8 u.r
                space

                2dup
                1- 15 and 1+
                begin
                        ?dup
                while
                        swap
                        dup c@
                        2 .r space
                        1+ swap 1-
                repeat
                drop

                2dup 1- 15 and 1+
                begin
                        ?dup
                while
                        swap
                        dup c@
                        dup 32 128 within if
                                emit
                        else
                                drop '.' emit
                        then
                        1+ swap 1-
                repeat
                drop
                cr

                dup 1- 15 and 1+
                tuck
                -
                >r + r>
        repeat

        drop
        base !
;

\ Usage:
\
\       VALUE
\       case
\       TEST1 of CASE1 endof
\       TEST2 of CASE2 endof
\       ...
\       TESTN of CASEN endof
\       DEFAULT-CASE
\       endcase
\
: case immediate
        0
;

: of immediate
        ' over ,
        ' = ,
        [compile] if
        ' drop ,
;

: endof immediate
        [compile] else
;

: endcase immediate
        ' drop ,
        begin
                ?dup
        while
                [compile] then
        repeat
;

: cfa>
        latest @
        begin
                ?dup
        while
                2dup swap
                < if
                        nip
                        exit
                then
                @
        repeat
        drop
        0
;

: see
        word find

        here @
        latest @
        begin
                2 pick
                over
                <>
        while
                nip
                dup @
        repeat

        drop
        swap

        ':' emit space dup id. space
        dup ?immediate if ." immediate " then

        >dfa

        begin
                2dup >
        while
                dup @

                case
                ' lit of
                        4 + dup @
                        .
                endof
                ' litstring of
                        [ char s ] literal emit '"' emit space
                        4 + dup @
                        swap 4 + swap
                        2dup tell
                        '"' emit space
                        + aligned
                        4 -
                endof
                ' 0branch of
                        ." 0branch ( "
                        4 + dup @
                        .
                        ." ) "
                endof
                ' branch of
                        ." branch ( "
                        4 + dup @
                        .
                        ." ) "
                endof
                ' ' of
                        [ char ' ] literal emit space
                        4 + dup @
                        cfa>
                        id. space
                endof
                ' exit of
                        2dup
                        4 +
                        <> if
                                ." exit "
                        then
                endof
                        \ Default case.
                        dup
                        cfa>
                        id. space
                endcase

                4+
        repeat

        ';' emit cr

        2drop
;

: :noname
        0 0 create
        here @
        docol ,
        ]
;

: ['] immediate
        ' lit ,
;

: exception-marker
        rdrop
        0
;

: catch ( xt -- exn? )
        dsp@ 4+ >r
        ' exception-marker 4+
        >r
        execute
;

: throw ( n -- )
        ?dup if
                rsp@
                begin
                        dup r0 4- <
                while
                        dup @
                        ' exception-marker 4+ = if
                                4+
                                rsp!

                                dup dup dup
                                r>
                                4-
                                swap over
                                !
                                dsp! exit
                        then
                        4+
                repeat

                drop

                case
                -1 of ( abort )
                        ." aborted" cr
                endof
                        \ Default case.
                        ." uncaught throw "
                        dup . cr
                endcase
                quit
        then
;

: abort ( -- )
        -1 throw
;

: print-stack-trace
        rsp@
        begin
                dup r0 4- <
        while
                dup @
                case
                ' exception-marker 4+ of
                        ." catch ( dsp="
                        4+ dup @ u.
                        ." ) "
                endof
                        \ Default case.
                        dup
                        cfa>
                        ?dup if
                                2dup
                                id.
                                [ char + ] literal emit
                                swap >dfa 4+ - .
                        then
                endcase
                4+
        repeat
        drop
        cr
;

: z" immediate ( -- str )
        state @ if      \ Compile mode.
                ' litstring ,
                here @
                0 ,
                begin
                        key
                        dup '"' <>
                while
                        c,
                repeat
                0 here @ c!
                1 here +!
                drop
                dup
                here @ swap -
                4-
                swap !
                align
                ' drop ,
        else            \ Immediate mode.
                here @
                begin
                        key
                        dup '"' <>
                while
                        over c!
                        1+
                repeat
                drop
                0 swap c!
                here @
        then
;

: strlen ( str -- len )
        dup
        begin
                dup c@ 0<>
        while
                1+
        repeat

        swap -
;

: cstring ( addr len -- str )
        swap over
        here @ swap
        cmove

        here @ +
        0 swap c!

        here @
;

: argc
        s0 @ @
;

: argv ( n -- addr len )
        1+ cells s0 @ +
        @
        dup strlen
;

: environ ( -- addr )
        argc
        2 +
        cells
        s0 @ +
;

: bye ( -- )
        0
        sys_exit
        syscall1
;

: get-brk
        0 sys_brk syscall1
;

: unused ( -- n )
        get-brk
        here @
        -
        4 /
;

: brk
        sys_brk syscall1
;

: morecore
        cells get-brk + brk
;

: r/o o_rdonly ;
: r/w o_rdwr ;

: open-file
        -rot
        cstring
        sys_open syscall2
        dup
        dup 0< if
                negate
        else
                drop 0
        then
;

: create-file
        o_creat or
        o_trunc or
        -rot
        cstring
        420 -rot
        sys_open syscall3
        dup
        dup 0< if
                negate
        else
                drop 0
        then
;

: close-file
        sys_close syscall1
        negate
;

: read-file
        >r swap r>
        sys_read syscall3

        dup
        dup 0< if
                negate
        else
                drop 0
        then
;

: perror
        tell
        ':' emit space
        ." errno="
        . cr
;

hex

: next immediate AD c, FF c, 20 c, ;

: ;code immediate
        [compile] next
        align
        latest @ dup
        hidden
        dup >dfa swap >cfa !
        [compile] [
;

: eax immediate 0 ;
: ecx immediate 1 ;
: edx immediate 2 ;
: ebx immediate 3 ;
: esp immediate 4 ;
: ebp immediate 5 ;
: esi immediate 6 ;
: edi immediate 7 ;

: push immediate 50 + c, ;
: pop immediate 58 + c, ;

: rdtsc immediate 0F c, 31 c, ;

decimal

: rdtsc
        rdtsc
        eax push
        edx push
;code

hex
: =next ( addr -- next? )
           dup c@ AD <> if drop false exit then
        1+ dup c@ FF <> if drop false exit then
        1+     c@ 20 <> if      false exit then
        true
;
decimal

: (inline)
        @
        begin
                dup =next not
        while
                dup c@ c,
                1+
        repeat
        drop
;

: inline immediate
        word find
        >cfa

        dup @ docol = if
                ." cannot inline FORTH words" cr abort
        then

        (inline)
;

hide =next

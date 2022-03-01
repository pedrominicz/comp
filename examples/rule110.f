(
- Porth episode 5: https://www.youtube.com/watch?v=Cm76TYPC-ow
- Rule 110 in Porth: https://github.com/tsoding/porth/blob/d650cbd19d3dfa872c8aefbcba7625bf5e2ddab9/examples/rule110.porth
- Rule 110 in C: https://gist.github.com/rexim/c595009436f87ca076e7c4a2fb92ce10
)

100 constant board-size

board-size allot constant board

board-size 1+ allot constant buffer

\ Fetch value at `board + offset`.
: b@ ( offset -- value ) board + c@ ;
\ Store value to `board + offset`.
: b! ( value offset -- ) board + c! ;

: main ( -- )
        1 board-size 1- b!

        '\n' board-size buffer + c!

        1
        begin
                dup board-size <
        while
                0
                begin
                        dup board-size <
                while
                        dup b@ if
                                [ char * ] literal
                        else
                                bl
                        then
                        over buffer + c!
                        1+
                repeat
                drop
                buffer board-size 1+ tell

                0 b@ 1 lshift
                1 b@ and                                \ pattern

                1                                       \ pattern 1
                begin
                        dup board-size 1- <             \ pattern index
                while
                        swap                            \ index pattern
                        1 lshift                        \ index (patter << 1)
                        7 and                           \ index ((pattern << 1) & 7)
                        over                            \ index ((pattern << 1) & 7) index
                        1+                              \ index ((pattern << 1) & 7) (index + 1)
                        b@                              \ index ((pattern << 1) & 7) board[index + 1]
                        or                              \ index (((pattern << 1) & 7) | board[index + 1])
                                                        \ index new_pattern
                        2dup                            \ index new_pattern index new_pattern
                        110                             \ index new_pattern index new_pattern 110
                        swap                            \ index new_pattern index 110 new_pattern
                        rshift                          \ index new_pattern index (110 >> new_pattern)
                        1 and                           \ index new_pattern index ((110 >> new_pattern) & 1)
                        swap                            \ index new_pattern ((110 >> new_pattern) & 1) index
                        b!                              \ index new_pattern
                        swap                            \ new_pattern index

                        1+                              \ new_pattern (index + 1)
                repeat
                2drop

                1+
        repeat
        drop
;

main

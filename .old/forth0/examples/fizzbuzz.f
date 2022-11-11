: fizzbuzz ( n -- )
        1 begin
                over
                over
                >=
        while
                dup 15 mod not if
                        ." fizzbuzz" cr
                else
                        dup 3 mod not if
                                ." fizz" cr
                        else
                                dup 5 mod not if
                                        ." buzz" cr
                                else
                                        dup . cr
                                then
                        then
                then
                1+
        repeat
        drop
        drop
;

: main ( -- )
        100 fizzbuzz
;

main

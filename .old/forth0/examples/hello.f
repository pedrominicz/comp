: main ( -- )
        s" Hello, world!"
        begin
                dup 0 >
        while
                swap
                dup @ emit
                1+
                swap
                1-
        repeat
        drop
        drop
;

main

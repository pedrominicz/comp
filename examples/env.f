: main ( -- )
        environ
        begin
                dup @ 0 <>
        while
                dup @ dup strlen tell cr
                4+
        repeat
        drop
;

main

\ Advent of Code 2015 Day 1 - Not Quite Lisp (Forth)

\ Get input filename from command line or use default
: get-input-filename ( -- c-addr u )
    argc @ 2 >= if
        \ Get the first argument (index 1)
        1 arg
    else
        s" ../data/input-full.txt"
    then ;

\ Calculate the floor Santa ends up on
: calculate-floor ( c-addr u -- floor )
    dup 0= if  \ handle empty string
        2drop 0 exit
    then
    0 swap 0 do           \ floor counter
        over i + c@       \ get character
        dup '(' = if
            drop 1 +      \ increment for '('
        else
            ')' = if
                1 -       \ decrement for ')'
            then
        then
    loop
    nip ;

\ Find position where Santa first enters basement (floor -1)
: find-basement-position ( c-addr u -- position | -1 )
    dup 0= if  \ handle empty string  
        2drop -1 exit
    then
    0 swap 0 do           \ current-floor counter
        over i + c@       \ get character
        dup '(' = if
            drop 1 +      \ increment for '('
        else
            ')' = if
                1 -       \ decrement for ')'
            then
        then
        dup -1 = if       \ check if we hit basement
            drop i 1 + unloop exit  \ return 1-based position
        then
    loop
    drop -1 ;             \ never entered basement

\ Read file into memory
: read-file-to-pad ( c-addr u -- c-addr2 u2 success? )
    r/o open-file throw
    dup >r
    pad 65535 rot read-file throw  \ read up to 65535 chars
    r> close-file throw
    pad swap true ;

\ Main program
: main
    get-input-filename
    read-file-to-pad
    if
        \ Calculate Part A
        2dup calculate-floor
        ." Santa ends up on floor " 0 .r cr
        
        \ Calculate Part B  
        find-basement-position
        dup -1 = if
            drop ." Santa never enters the basement" cr
        else
            ." The first character that causes Santa to enter the basement is at position " 0 .r cr
        then
    else
        ." Error: Could not read input file" cr
    then ;

main bye
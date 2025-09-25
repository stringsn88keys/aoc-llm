\ Advent of Code 2015 Day 1 - Forth Tests

\ Test framework words
variable test-count
variable passed-count

: reset-tests ( -- )
    0 test-count !
    0 passed-count ! ;

: assert-equal ( actual expected -- )
    test-count @ 1 + test-count !
    = if
        passed-count @ 1 + passed-count !
        ." ✓ "
    else
        ." ✗ "
    then ;

: show-test-summary ( -- )
    cr ." Tests run: " test-count @ . cr
    ." Tests passed: " passed-count @ . cr
    ." Tests failed: " test-count @ passed-count @ - . cr
    passed-count @ test-count @ = if
        ." All tests passed! ✓" cr
    else
        ." Some tests failed! ✗" cr
    then ;

\ Solution words (copied from solution.fs)
: calculate-floor ( c-addr u -- floor )
    dup 0= if  \ handle empty string
        2drop 0 exit
    then
    0 swap 0 do           
        over i + c@       
        dup '(' = if
            drop 1 +      
        else
            ')' = if
                1 -       
            then
        then
    loop
    nip ;

: find-basement-position ( c-addr u -- position | -1 )
    dup 0= if  \ handle empty string  
        2drop -1 exit
    then
    0 swap 0 do           
        over i + c@       
        dup '(' = if
            drop 1 +      
        else
            ')' = if
                1 -       
            then
        then
        dup -1 = if       
            drop i 1 + unloop exit  
        then
    loop
    drop -1 ;             

\ Test cases
: run-tests ( -- )
    reset-tests
    
    cr ." Testing calculate-floor:" cr
    
    \ Test 1: (()) should result in floor 0
    s" (())" calculate-floor 0 assert-equal
    ." (()) should result in floor 0" cr
    
    \ Test 2: ()() should result in floor 0  
    s" ()()" calculate-floor 0 assert-equal
    ." ()() should result in floor 0" cr
    
    \ Test 3: ((( should result in floor 3
    s" (((" calculate-floor 3 assert-equal
    ." ((( should result in floor 3" cr
    
    \ Test 4: ))) should result in floor -3
    s" )))" calculate-floor -3 assert-equal  
    ." ))) should result in floor -3" cr
    
    \ Test 5: )())()) should result in floor -3
    s" )())())" calculate-floor -3 assert-equal
    ." )())()) should result in floor -3" cr
    
    cr ." Testing find-basement-position:" cr
    
    \ Test 6: ) should enter basement at position 1
    s" )" find-basement-position 1 assert-equal
    ." ) should enter basement at position 1" cr
    
    \ Test 7: ()()) should enter basement at position 5
    s" ()())" find-basement-position 5 assert-equal
    ." ()()) should enter basement at position 5" cr
    
    \ Test 8: ((( should never enter basement
    s" (((" find-basement-position -1 assert-equal
    ." ((( should never enter basement" cr
    
    \ Test 9: Empty string should result in floor 0
    s" " calculate-floor 0 assert-equal
    ." Empty string should result in floor 0" cr
    
    \ Test 10: Empty string should never enter basement
    s" " find-basement-position -1 assert-equal
    ." Empty string should never enter basement" cr
    
    show-test-summary ;

run-tests bye
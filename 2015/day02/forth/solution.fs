\ Advent of Code 2015 Day 2 Part A - Wrapping Paper Calculator

\ Helper word to find minimum of two numbers (avoid redefinition)
: my-min ( a b -- min )
    2dup > if swap then drop
;

\ Calculate wrapping paper needed for a box with dimensions l x w x h
\ Stack: ( l w h -- paper )
: calculate-wrapping-paper ( l w h -- paper )
    2dup * >r              \ Save w*h, stack: l w h  R: w*h
    over over * >r         \ Save l*h, stack: l w h  R: w*h l*h  
    rot 2dup * >r          \ Save l*w, stack: w h l   R: w*h l*h l*w
    
    \ Calculate surface area: 2*l*w + 2*w*h + 2*h*l
    r@ 2 *                 \ 2*l*w
    r> r@ 2 * +            \ + 2*w*h  
    r> r@ 2 * +            \ + 2*h*l
    
    \ Find minimum of the three sides for extra paper
    r@ r@ r@ 
    my-min my-min          \ minimum side area
    +                      \ surface area + minimum side
    r> drop r> drop r> drop \ clean up return stack
;

\ Parse a single number from input buffer, advance position
variable parse-pos
variable input-buffer
variable input-length

\ Initialize parsing
: init-parse ( addr len -- )
    input-length ! input-buffer ! 0 parse-pos !
;

\ Get character at current position
: current-char ( -- char )
    input-buffer @ parse-pos @ + c@
;

\ Skip non-digits
: skip-non-digits ( -- )
    begin
        parse-pos @ input-length @ < 
        current-char dup 48 >= over 57 <= and 0= and
    while
        parse-pos @ 1+ parse-pos !
        drop
    repeat
;

\ Parse one number
: parse-number ( -- n )
    0 >r
    skip-non-digits
    begin
        parse-pos @ input-length @ < 
        current-char dup 48 >= over 57 <= and
    while
        48 - r> 10 * + >r
        parse-pos @ 1+ parse-pos !
        drop
    repeat
    r>
;

\ Parse dimensions from string like "2x3x4"
: parse-dimensions ( addr len -- l w h )
    init-parse
    parse-number parse-number parse-number
;

\ Test the solution with examples
: test-examples ( -- )
    cr ." Running Forth Day 2 tests..." cr
    
    \ Test example 1: 2x3x4 should give 58
    2 3 4 calculate-wrapping-paper 58 = if
        ." ✓ Example 1 (2x3x4 = 58)" cr
    else
        ." ✗ Example 1 failed" cr
    then
    
    \ Test example 2: 1x1x10 should give 43  
    1 1 10 calculate-wrapping-paper 43 = if
        ." ✓ Example 2 (1x1x10 = 43)" cr
    else
        ." ✗ Example 2 failed" cr
    then
    
    \ Test cube: 2x2x2 should give 28
    2 2 2 calculate-wrapping-paper 28 = if
        ." ✓ Cube (2x2x2 = 28)" cr
    else
        ." ✗ Cube test failed" cr
    then
    
    \ Test unit cube: 1x1x1 should give 7
    1 1 1 calculate-wrapping-paper 7 = if
        ." ✓ Unit cube (1x1x1 = 7)" cr
    else
        ." ✗ Unit cube test failed" cr
    then
    
    \ Test parsing
    s" 2x3x4" parse-dimensions
    4 = swap 3 = swap 2 = and and if
        ." ✓ Parse 2x3x4" cr
    else
        ." ✗ Parse 2x3x4 failed" cr
    then
    
    ." All tests completed!" cr
;

\ Main program
: main ( -- )
    cr ." Advent of Code 2015 Day 2 Part A - Forth Solution" cr
    test-examples
    
    \ Calculate examples total
    2 3 4 calculate-wrapping-paper
    1 1 10 calculate-wrapping-paper
    +
    ." Total for examples: " . ." square feet" cr
;

\ Auto-run when loaded
main
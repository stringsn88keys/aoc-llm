\ Test file for Advent of Code 2015 Day 2 Part A

\ Load the solution
include solution.fs

\ Additional comprehensive tests
: run-all-tests ( -- )
    cr ." === Running Comprehensive Forth Tests ===" cr
    
    \ All tests are already run by solution.fs when loaded
    ." Additional verification:" cr
    
    \ Verify calculation components
    2 3 4 
    ." Testing 2x3x4:" cr
    ." - Sides: 2*3=" 2 3 * . 
    ." 3*4=" 3 4 * .
    ." 2*4=" 2 4 * . cr
    ." - Surface area: " 2 3 * 2 * 3 4 * 2 * + 2 4 * 2 * + . cr
    ." - Smallest side: " 2 3 * 3 4 * 2 4 * my-min my-min . cr
    ." - Total: " 2 3 4 calculate-wrapping-paper . cr
    
    cr ." === Tests Complete ===" cr
;

\ Run comprehensive tests
run-all-tests
-module(test_solution).
-export([run_tests/0]).

% Import the solution module functions
-import(solution, [calculate_wrapping_paper/3, parse_dimensions/1]).

% Simple test runner
run_test(Name, Expected, Actual) ->
    case Expected =:= Actual of
        true -> 
            io:format("✓ ~s~n", [Name]);
        false -> 
            io:format("✗ ~s - Expected: ~p, Got: ~p~n", [Name, Expected, Actual])
    end.

run_tests() ->
    io:format("Running Erlang Day 2 tests...~n"),
    
    % Test calculate_wrapping_paper with examples
    run_test("Example 1 (2x3x4 = 58)", 58, calculate_wrapping_paper(2, 3, 4)),
    run_test("Example 2 (1x1x10 = 43)", 43, calculate_wrapping_paper(1, 1, 10)),
    
    % Test edge cases
    run_test("Cube (2x2x2 = 28)", 28, calculate_wrapping_paper(2, 2, 2)),
    run_test("Unit cube (1x1x1 = 7)", 7, calculate_wrapping_paper(1, 1, 1)),
    
    % Test parse_dimensions
    run_test("Parse 2x3x4", {2, 3, 4}, parse_dimensions("2x3x4")),
    run_test("Parse 1x1x10", {1, 1, 10}, parse_dimensions("1x1x10")),
    run_test("Parse 100x200x300", {100, 200, 300}, parse_dimensions("100x200x300")),
    
    io:format("All tests completed!~n").
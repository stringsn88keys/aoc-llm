-module(santa_floor_tests).
-include_lib("eunit/include/eunit.hrl").

% Test Part A examples
examples_from_problem_part_a_test() ->
    % (()) and ()() both result in floor 0
    ?assertEqual(0, santa_floor:calculate_floor("(())")),
    ?assertEqual(0, santa_floor:calculate_floor("()()")),
    
    % ((( and (()(()( both result in floor 3
    ?assertEqual(3, santa_floor:calculate_floor("(((")),
    ?assertEqual(3, santa_floor:calculate_floor("(()(()(")),
    
    % ))((((( also results in floor 3
    ?assertEqual(3, santa_floor:calculate_floor("))((((( ")),
    
    % ()) and ))( both result in floor -1 (the first basement level)
    ?assertEqual(-1, santa_floor:calculate_floor("())")),
    ?assertEqual(-1, santa_floor:calculate_floor("))(")),
    
    % ))) and )())()) both result in floor -3
    ?assertEqual(-3, santa_floor:calculate_floor(")))")),
    ?assertEqual(-3, santa_floor:calculate_floor(")())())")).

empty_string_test() ->
    % Empty string should result in floor 0
    ?assertEqual(0, santa_floor:calculate_floor("")).

single_parenthesis_test() ->
    % Single open parenthesis should be floor 1
    ?assertEqual(1, santa_floor:calculate_floor("(")),
    
    % Single close parenthesis should be floor -1
    ?assertEqual(-1, santa_floor:calculate_floor(")")).
    
% Test Part B examples
examples_from_problem_part_b_test() ->
    % ) causes him to enter the basement at character position 1
    ?assertEqual(1, santa_floor:find_basement_position(")")),
    
    % ()()) causes him to enter the basement at character position 5
    ?assertEqual(5, santa_floor:find_basement_position("()())")).

never_enter_basement_test() ->
    % Test cases where Santa never enters the basement
    ?assertEqual(-1, santa_floor:find_basement_position("(")),  % Stays at floor 1
    ?assertEqual(-1, santa_floor:find_basement_position("(((")),  % Stays positive
    ?assertEqual(-1, santa_floor:find_basement_position("(()")),  % Ends at floor 1
    ?assertEqual(-1, santa_floor:find_basement_position("")).  % Empty string

immediate_basement_entry_test() ->
    % Direct entry to basement
    ?assertEqual(1, santa_floor:find_basement_position(")")),
    ?assertEqual(1, santa_floor:find_basement_position("))")).  % First ) takes him to -1

later_basement_entry_test() ->
    % Later entry to basement
    % ()()) causes him to enter the basement at character position 5
    ?assertEqual(5, santa_floor:find_basement_position("()())")),
    % A longer example: (((())))) -> floors 1,2,3,4,3,2,1,0,-1 at position 9
    ?assertEqual(9, santa_floor:find_basement_position("(((()))))")).
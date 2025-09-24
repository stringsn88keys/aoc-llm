@echo off
echo -module(santa_floor). > solution_fixed.erl
echo -export([calculate_floor/1, find_basement_position/1, main/1]). >> solution_fixed.erl
echo. >> solution_fixed.erl
echo % Calculate the final floor based on instructions >> solution_fixed.erl
echo calculate_floor(Instructions) ->
echo     calculate_floor(Instructions, 0). >> solution_fixed.erl
echo. >> solution_fixed.erl
echo calculate_floor([], Floor) ->
echo     Floor; >> solution_fixed.erl
echo calculate_floor([H|T], Floor) ->
echo     NewFloor = case H of
echo         40 -> Floor + 1;  %% '(' character
echo         41 -> Floor - 1;  %% ')' character
echo         _ -> Floor
echo     end,
echo     calculate_floor(T, NewFloor). >> solution_fixed.erl
echo. >> solution_fixed.erl
echo % Find the position of the first character that causes Santa to enter the basement (floor -1)
echo find_basement_position(Instructions) ->
echo     find_basement_position(Instructions, 0, 1). >> solution_fixed.erl
echo. >> solution_fixed.erl
echo find_basement_position([], _Floor, _Position) ->
echo     -1; >> solution_fixed.erl
echo find_basement_position([H|T], Floor, Position) ->
echo     NewFloor = case H of
echo         40 -> Floor + 1;  %% '(' character
echo         41 -> Floor - 1;  %% ')' character
echo         _ -> Floor
echo     end,
echo     if
echo         NewFloor == -1 ->
echo             Position;
echo         true ->
echo             find_basement_position(T, NewFloor, Position + 1)
echo     end. >> solution_fixed.erl
echo. >> solution_fixed.erl
echo % Main function that handles file input and outputs results
echo main(Args) ->
echo     InputFile = case Args of
echo         [] -> "input.txt";
echo         [File] -> File
echo     end,
echo.
echo     case file:read_file(InputFile) of
echo         {ok, BinContent} ->
echo             Instructions = string:trim(binary_to_list(BinContent)),
echo.
echo             % Part A: Calculate final floor
echo             FinalFloor = calculate_floor(Instructions),
echo             io:format("Santa ends up on floor ~p~n", [FinalFloor]),
echo.
echo             % Part B: Find position where Santa first enters basement
echo             BasementPos = find_basement_position(Instructions),
echo             if
echo                 BasementPos =/= -1 ->
echo                     io:format("The first character that causes Santa to enter the basement is at position ~p~n", [BasementPos]);
echo                 true ->
echo                     io:format("Santa never enters the basement~n")
echo             end;
echo         {error, _Reason} ->
echo             io:format("~s not found. Please create the file with the puzzle input.~n", [InputFile]),
echo             io:format("Example usage: calculate_floor(\"(((\") returns ~p~n", [calculate_floor("(((")]),
echo             io:format("Example usage: find_basement_position(\"()())\") returns ~p~n", [find_basement_position("()())"])
echo     end. >> solution_fixed.erl
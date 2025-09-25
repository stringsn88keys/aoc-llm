-module(solution).
-export([calculate_floor/1, find_basement_position/1, main/1]).

% Calculate the final floor based on instructions
calculate_floor(Instructions) ->
    calculate_floor(Instructions, 0).

calculate_floor([], Floor) ->
    Floor;
calculate_floor([H|T], Floor) ->
    NewFloor = case H of
        40 -> Floor + 1;  % '(' character
        41 -> Floor - 1;  % ')' character
        _ -> Floor
    end,
    calculate_floor(T, NewFloor).

% Find the position of the first character that causes Santa to enter the basement (floor -1)
find_basement_position(Instructions) ->
    find_basement_position(Instructions, 0, 1).

find_basement_position([], _Floor, _Position) ->
    -1;
find_basement_position([H|T], Floor, Position) ->
    NewFloor = case H of
        40 -> Floor + 1;  % '(' character
        41 -> Floor - 1;  % ')' character
        _ -> Floor
    end,
    if
        NewFloor == -1 ->
            Position;
        true ->
            find_basement_position(T, NewFloor, Position + 1)
    end.

% Main function that handles file input and outputs results
main(Args) ->
    InputFile = case Args of
        [] -> "input.txt";
        [File] -> File
    end,
    
    case file:read_file(InputFile) of
        {ok, BinContent} ->
            Instructions = string:trim(binary_to_list(BinContent)),
            
            % Part A: Calculate final floor
            FinalFloor = calculate_floor(Instructions),
            io:format("Santa ends up on floor ~p~n", [FinalFloor]),
            
            % Part B: Find position where Santa first enters basement
            BasementPos = find_basement_position(Instructions),
            if
                BasementPos =/= -1 ->
                    io:format("The first character that causes Santa to enter the basement is at position ~p~n", [BasementPos]);
                true ->
                    io:format("Santa never enters the basement~n")
            end;
        {error, _Reason} ->
            io:format("~s not found. Please create the file with the puzzle input.~n", [InputFile]),
            io:format("Example usage: calculate_floor(\"(((\") returns ~p~n", [calculate_floor("(((")]),
            io:format("Example usage: find_basement_position(\"()())\") returns ~p~n", [find_basement_position("()())")])
    end.
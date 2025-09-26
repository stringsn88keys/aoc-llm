-module(solution).
-export([main/0, main/1, calculate_wrapping_paper/3, parse_dimensions/1, solve_file/1]).

% Calculate wrapping paper needed for a box with dimensions L x W x H
calculate_wrapping_paper(L, W, H) ->
    SurfaceArea = 2*L*W + 2*W*H + 2*H*L,
    Sides = [L*W, W*H, H*L],
    SmallestSide = lists:min(Sides),
    SurfaceArea + SmallestSide.

% Parse dimensions from string like "2x3x4"
parse_dimensions(Line) ->
    CleanLine = string:strip(Line, right, $\n),
    Parts = string:tokens(CleanLine, "x"),
    [L, W, H] = [list_to_integer(X) || X <- Parts],
    {L, W, H}.

% Solve for entire file
solve_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    NonEmptyLines = [L || L <- Lines, L =/= ""],
    Total = lists:sum([
        begin
            {L, W, H} = parse_dimensions(Line),
            calculate_wrapping_paper(L, W, H)
        end || Line <- NonEmptyLines
    ]),
    Total.

% Main function that accepts command line arguments or runs with default behavior
main([InputFile|_]) ->
    % Called with command line argument
    case filelib:is_regular(InputFile) of
        true ->
            Result = solve_file(InputFile),
            io:format("The elves should order ~p square feet of wrapping paper~n", [Result]);
        false ->
            io:format("Input file not found: ~s~n", [InputFile])
    end;
main([]) ->
    % Called with empty argument list - find input file automatically
    main().

main() ->
    case file:get_cwd() of
        {ok, Cwd} ->
            io:format("Current working directory: ~s~n", [Cwd]);
        {error, Reason} ->
            io:format("Error getting cwd: ~p~n", [Reason])
    end,
    
    % Check if we're in the right directory and find the input file
    InputFile = case filelib:is_regular("../data/input.txt") of
        true -> "../data/input.txt";
        false -> 
            case filelib:is_regular("input.txt") of
                true -> "input.txt";
                false -> 
                    case filelib:is_regular("../python/input.txt") of
                        true -> "../python/input.txt";
                        false -> "input-not-found.txt"
                    end
            end
    end,
    
    case filelib:is_regular(InputFile) of
        true ->
            Result = solve_file(InputFile),
            io:format("The elves should order ~p square feet of wrapping paper~n", [Result]);
        false ->
            io:format("Input file not found at: ~s~n", [InputFile]),
            % Test with examples
            Example1 = calculate_wrapping_paper(2, 3, 4),
            Example2 = calculate_wrapping_paper(1, 1, 10),
            io:format("Example 1 (2x3x4): ~p square feet~n", [Example1]),
            io:format("Example 2 (1x1x10): ~p square feet~n", [Example2]),
            io:format("Total for examples: ~p square feet~n", [Example1 + Example2])
    end.
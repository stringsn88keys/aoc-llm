-module(solution).
-export([main/0, main/1, calculate_wrapping_paper/3, calculate_ribbon/3, parse_dimensions/1, solve_file/1, solve_file_part_b/1]).

% Calculate wrapping paper needed for a box with dimensions L x W x H
calculate_wrapping_paper(L, W, H) ->
    SurfaceArea = 2*L*W + 2*W*H + 2*H*L,
    Sides = [L*W, W*H, H*L],
    SmallestSide = lists:min(Sides),
    SurfaceArea + SmallestSide.

% Calculate ribbon needed for a box with dimensions L x W x H
calculate_ribbon(L, W, H) ->
    % Ribbon for wrapping: shortest distance around sides (smallest perimeter)
    Perimeters = [2*(L+W), 2*(W+H), 2*(H+L)],
    WrappingRibbon = lists:min(Perimeters),
    % Ribbon for bow: volume of the present
    BowRibbon = L * W * H,
    WrappingRibbon + BowRibbon.

% Parse dimensions from string like "2x3x4"
parse_dimensions(Line) ->
    CleanLine = string:strip(Line, right, $\n),
    Parts = string:tokens(CleanLine, "x"),
    [L, W, H] = [list_to_integer(X) || X <- Parts],
    {L, W, H}.

% Solve for entire file (Part A)
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

% Solve for entire file (Part B)
solve_file_part_b(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    NonEmptyLines = [L || L <- Lines, L =/= ""],
    Total = lists:sum([
        begin
            {L, W, H} = parse_dimensions(Line),
            calculate_ribbon(L, W, H)
        end || Line <- NonEmptyLines
    ]),
    Total.

% Main function that accepts command line arguments or runs with default behavior
main([InputFile|_]) ->
    % Called with command line argument
    case filelib:is_regular(InputFile) of
        true ->
            PaperResult = solve_file(InputFile),
            RibbonResult = solve_file_part_b(InputFile),
            io:format("The elves should order ~p square feet of wrapping paper~n", [PaperResult]),
            io:format("The elves should order ~p feet of ribbon~n", [RibbonResult]);
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
            PaperResult = solve_file(InputFile),
            RibbonResult = solve_file_part_b(InputFile),
            io:format("The elves should order ~p square feet of wrapping paper~n", [PaperResult]),
            io:format("The elves should order ~p feet of ribbon~n", [RibbonResult]);
        false ->
            io:format("Input file not found at: ~s~n", [InputFile]),
            % Test with examples
            PaperExample1 = calculate_wrapping_paper(2, 3, 4),
            PaperExample2 = calculate_wrapping_paper(1, 1, 10),
            RibbonExample1 = calculate_ribbon(2, 3, 4),
            RibbonExample2 = calculate_ribbon(1, 1, 10),
            io:format("Paper Example 1 (2x3x4): ~p square feet~n", [PaperExample1]),
            io:format("Paper Example 2 (1x1x10): ~p square feet~n", [PaperExample2]),
            io:format("Total paper for examples: ~p square feet~n", [PaperExample1 + PaperExample2]),
            io:format("Ribbon Example 1 (2x3x4): ~p feet~n", [RibbonExample1]),
            io:format("Ribbon Example 2 (1x1x10): ~p feet~n", [RibbonExample2]),
            io:format("Total ribbon for examples: ~p feet~n", [RibbonExample1 + RibbonExample2])
    end.
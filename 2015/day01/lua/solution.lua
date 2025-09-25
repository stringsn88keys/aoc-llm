-- Advent of Code 2015 Day 1 - Lua Solution

-- Calculate the final floor based on instructions
function calculate_floor(instructions)
    local floor = 0
    for i = 1, #instructions do
        local char = instructions:sub(i, i)
        if char == '(' then
            floor = floor + 1
        elseif char == ')' then
            floor = floor - 1
        end
    end
    return floor
end

-- Find the position of the first character that causes Santa to enter the basement (floor -1)
function find_basement_position(instructions)
    local floor = 0
    for i = 1, #instructions do
        local char = instructions:sub(i, i)
        if char == '(' then
            floor = floor + 1
        elseif char == ')' then
            floor = floor - 1
        end
        
        if floor == -1 then
            return i  -- Lua uses 1-based indexing
        end
    end
    return -1  -- Never entered the basement
end

-- Main function to handle file input and output results
function main()
    -- Get input file from command line argument or use default
    local input_file = arg and arg[1] or "../data/input.txt"
    
    -- Read the file
    local file = io.open(input_file, "r")
    if not file then
        print(input_file .. " not found. Please create the file with the puzzle input.")
        print("Example usage: calculate_floor('(((') returns " .. calculate_floor("((("))
        print("Example usage: find_basement_position('()())') returns " .. find_basement_position("()())"))
        return
    end
    
    local instructions = file:read("*all")
    file:close()
    
    -- Remove any whitespace/newlines
    instructions = instructions:gsub("%s", "")
    
    -- Part A: Calculate final floor
    local final_floor = calculate_floor(instructions)
    print("Santa ends up on floor " .. final_floor)
    
    -- Part B: Find position where Santa first enters basement
    local basement_pos = find_basement_position(instructions)
    if basement_pos ~= -1 then
        print("The first character that causes Santa to enter the basement is at position " .. basement_pos)
    else
        print("Santa never enters the basement")
    end
end

-- Run main function if this script is executed directly
if arg and arg[0] and arg[0]:match("solution%.lua$") then
    main()
end
#!/usr/bin/env lua

-- Calculate wrapping paper needed for a box with dimensions l x w x h
function calculate_wrapping_paper(l, w, h)
    local surface_area = 2*l*w + 2*w*h + 2*h*l
    local sides = {l*w, w*h, h*l}
    local smallest_side = math.min(sides[1], sides[2], sides[3])
    return surface_area + smallest_side
end

-- Parse dimensions from string like "2x3x4"
function parse_dimensions(line)
    line = line:gsub("^%s*(.-)%s*$", "%1") -- trim whitespace
    local parts = {}
    for part in line:gmatch("[^x]+") do
        table.insert(parts, tonumber(part))
    end
    return parts[1], parts[2], parts[3]
end

-- Solve for entire file
function solve_file(filename)
    local file = io.open(filename, "r")
    if not file then
        print("Error: Could not open file " .. filename)
        return nil
    end
    
    local total = 0
    for line in file:lines() do
        if line and line:match("%S") then -- non-empty line
            local l, w, h = parse_dimensions(line)
            if l and w and h then
                total = total + calculate_wrapping_paper(l, w, h)
            end
        end
    end
    file:close()
    return total
end

-- Check if file exists
function file_exists(filename)
    local file = io.open(filename, "r")
    if file then
        file:close()
        return true
    end
    return false
end

-- Main function
function main()
    -- Try to find input file
    local input_file
    local possible_files = {
        "../data/input.txt",
        "input.txt", 
        "../python/input.txt"
    }
    
    for _, file in ipairs(possible_files) do
        if file_exists(file) then
            input_file = file
            break
        end
    end
    
    if input_file then
        local result = solve_file(input_file)
        if result then
            print("The elves should order " .. result .. " square feet of wrapping paper")
        end
    else
        print("Input file not found, running examples:")
        -- Test with examples
        local example1 = calculate_wrapping_paper(2, 3, 4)
        local example2 = calculate_wrapping_paper(1, 1, 10)
        print("Example 1 (2x3x4): " .. example1 .. " square feet")
        print("Example 2 (1x1x10): " .. example2 .. " square feet")
        print("Total for examples: " .. (example1 + example2) .. " square feet")
    end
end

-- Run if this file is executed directly
if arg and arg[0] and arg[0]:match("solution%.lua$") then
    main()
end
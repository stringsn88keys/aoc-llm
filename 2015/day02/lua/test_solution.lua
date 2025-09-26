#!/usr/bin/env lua

-- Import solution functions
require("solution")

-- Simple test framework
local tests_passed = 0
local tests_total = 0

function run_test(name, expected, actual)
    tests_total = tests_total + 1
    if expected == actual then
        print("✓ " .. name)
        tests_passed = tests_passed + 1
    else
        print("✗ " .. name .. " - Expected: " .. tostring(expected) .. ", Got: " .. tostring(actual))
    end
end

function main()
    print("Running Lua Day 2 tests...")
    
    -- Test calculate_wrapping_paper with examples
    run_test("Example 1 (2x3x4 = 58)", 58, calculate_wrapping_paper(2, 3, 4))
    run_test("Example 2 (1x1x10 = 43)", 43, calculate_wrapping_paper(1, 1, 10))
    
    -- Test edge cases
    run_test("Cube (2x2x2 = 28)", 28, calculate_wrapping_paper(2, 2, 2))
    run_test("Unit cube (1x1x1 = 7)", 7, calculate_wrapping_paper(1, 1, 1))
    
    -- Test parse_dimensions
    local l1, w1, h1 = parse_dimensions("2x3x4")
    run_test("Parse 2x3x4 - length", 2, l1)
    run_test("Parse 2x3x4 - width", 3, w1)  
    run_test("Parse 2x3x4 - height", 4, h1)
    
    local l2, w2, h2 = parse_dimensions("1x1x10")
    run_test("Parse 1x1x10 - length", 1, l2)
    run_test("Parse 1x1x10 - width", 1, w2)
    run_test("Parse 1x1x10 - height", 10, h2)
    
    print("Tests completed: " .. tests_passed .. "/" .. tests_total .. " passed")
end

main()
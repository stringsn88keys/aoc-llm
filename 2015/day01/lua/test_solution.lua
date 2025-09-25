-- Advent of Code 2015 Day 1 - Lua Tests

-- Load the solution module
require('solution')

-- Simple test framework
local tests_run = 0
local tests_passed = 0

function assert_equal(actual, expected, message)
    tests_run = tests_run + 1
    if actual == expected then
        tests_passed = tests_passed + 1
        print("✓ " .. message)
    else
        print("✗ " .. message .. " (expected " .. tostring(expected) .. ", got " .. tostring(actual) .. ")")
    end
end

-- Test Part A examples
print("Testing Part A - Calculate Floor:")

-- (()) and ()() both result in floor 0
assert_equal(calculate_floor("(())"), 0, "(()) should result in floor 0")
assert_equal(calculate_floor("()()"), 0, "()() should result in floor 0")

-- ((( and (()(()( both result in floor 3
assert_equal(calculate_floor("((("), 3, "((( should result in floor 3")
assert_equal(calculate_floor("(()(()("), 3, "(()(()( should result in floor 3")

-- ))((((( also results in floor 3
assert_equal(calculate_floor("))((((( "), 3, "))((((( should result in floor 3")

-- ()) and ))( both result in floor -1 (the first basement level)
assert_equal(calculate_floor("())"), -1, "()) should result in floor -1")
assert_equal(calculate_floor("))("), -1, "))( should result in floor -1")

-- ))) and )())()) both result in floor -3
assert_equal(calculate_floor(")))"), -3, "))) should result in floor -3")
assert_equal(calculate_floor(")())())"), -3, ")())()) should result in floor -3")

-- Test empty string
assert_equal(calculate_floor(""), 0, "Empty string should result in floor 0")

-- Test single characters
assert_equal(calculate_floor("("), 1, "Single ( should result in floor 1")
assert_equal(calculate_floor(")"), -1, "Single ) should result in floor -1")

print("\nTesting Part B - Find Basement Position:")

-- Test Part B examples
assert_equal(find_basement_position(")"), 1, ") should enter basement at position 1")
assert_equal(find_basement_position("()())"), 5, "()()) should enter basement at position 5")

-- Test cases where Santa never enters the basement
assert_equal(find_basement_position("("), -1, "( should never enter basement")
assert_equal(find_basement_position("((("), -1, "((( should never enter basement")
assert_equal(find_basement_position("(())"), -1, "(()) should never enter basement")
assert_equal(find_basement_position(""), -1, "Empty string should never enter basement")

-- Test immediate basement entry
assert_equal(find_basement_position("))"), 1, ")) should enter basement at position 1")

-- Test later basement entry
assert_equal(find_basement_position("(((()))))) "), 9, "Complex example should enter basement at position 9")

-- Test with actual test input file
local test_file = io.open("../data/input-test-a.txt", "r")
if test_file then
    local test_input = test_file:read("*all")
    test_file:close()
    test_input = test_input:gsub("%s", "")
    assert_equal(calculate_floor(test_input), 0, "input-test-a.txt should result in floor 0")
else
    print("Note: ../data/input-test-a.txt not found, skipping file test")
end

-- Print test summary
print("\n" .. string.rep("=", 50))
print("Test Summary:")
print("Tests run: " .. tests_run)
print("Tests passed: " .. tests_passed)
print("Tests failed: " .. (tests_run - tests_passed))

if tests_passed == tests_run then
    print("All tests passed! ✓")
    os.exit(0)
else
    print("Some tests failed! ✗")
    os.exit(1)
end
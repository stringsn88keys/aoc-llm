# Tests for the solution.cr file
require "./solution"

# Test Part A examples
def test_examples_from_problem_part_a
  # (()) and ()() both result in floor 0
  raise "Expected (()) to result in floor 0" unless calculate_floor("(())") == 0
  raise "Expected ()() to result in floor 0" unless calculate_floor("()()") == 0
  
  # ((( and (()(()( both result in floor 3
  raise "Expected ((( to result in floor 3" unless calculate_floor("(((") == 3
  raise "Expected (()(()( to result in floor 3" unless calculate_floor("(()(()(") == 3
  
  # ))((((( also results in floor 3
  raise "Expected ))(((((  to result in floor 3" unless calculate_floor("))((((( ") == 3
  
  # ()) and ))( both result in floor -1 (the first basement level)
  raise "Expected ()) to result in floor -1" unless calculate_floor("())") == -1
  raise "Expected ))( to result in floor -1" unless calculate_floor("))(") == -1
  
  # ))) and )())()) both result in floor -3
  raise "Expected ))) to result in floor -3" unless calculate_floor(")))") == -3
  raise "Expected )())()) to result in floor -3" unless calculate_floor(")())())") == -3
  
  puts "Part A examples test passed!"
end

def test_empty_string
  # Empty string should result in floor 0
  raise "Expected empty string to result in floor 0" unless calculate_floor("") == 0
  puts "Empty string test passed!"
end

def test_single_parenthesis
  # Single open parenthesis should be floor 1
  raise "Expected ( to result in floor 1" unless calculate_floor("(") == 1
  
  # Single close parenthesis should be floor -1
  raise "Expected ) to result in floor -1" unless calculate_floor(")") == -1
  puts "Single parenthesis test passed!"
end

# Test Part B examples
def test_examples_from_problem_part_b
  # ) causes him to enter the basement at character position 1
  raise "Expected ) to cause basement entry at position 1" unless find_basement_position(")") == 1
  
  # ()()) causes him to enter the basement at character position 5
  raise "Expected ()()) to cause basement entry at position 5" unless find_basement_position("()())") == 5
  puts "Part B examples test passed!"
end

def test_never_enter_basement
  # Test cases where Santa never enters the basement
  raise "Expected ( not to enter basement" unless find_basement_position("(") == -1  # Stays at floor 1
  raise "Expected ((( not to enter basement" unless find_basement_position("(((") == -1  # Stays positive
  raise "Expected (() not to enter basement" unless find_basement_position("(()") == -1  # Ends at floor 1
  raise "Expected empty string not to enter basement" unless find_basement_position("") == -1  # Empty string
  puts "Never enter basement test passed!"
end

def test_immediate_basement_entry
  # Direct entry to basement
  raise "Expected ) to cause basement entry at position 1" unless find_basement_position(")") == 1
  raise "Expected )) to cause basement entry at position 1" unless find_basement_position("))") == 1  # First ) takes him to -1
  puts "Immediate basement entry test passed!"
end

def test_later_basement_entry
  # Later entry to basement
  # ()()) causes him to enter the basement at character position 5
  raise "Expected ()()) to cause basement entry at position 5" unless find_basement_position("()())") == 5
  # A longer example: (((())))) -> floors 1,2,3,4,3,2,1,0,-1 at position 9
  raise "Expected (((())))) to cause basement entry at position 9" unless find_basement_position("(((()))))") == 9
  puts "Later basement entry test passed!"
end

# Run all tests
def run_tests
  puts "Running Crystal tests for Day 1 solution..."
  
  test_examples_from_problem_part_a
  test_empty_string
  test_single_parenthesis
  test_examples_from_problem_part_b
  test_never_enter_basement
  test_immediate_basement_entry
  test_later_basement_entry
  
  puts "All Crystal tests passed!"
end

run_tests
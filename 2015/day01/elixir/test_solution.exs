# Process.put(:elixir_test_mode, true)
Code.require_file("solution.exs", __DIR__)
ExUnit.start()

defmodule SolutionTest do
  use ExUnit.Case

  import Solution

  test "examples from problem part a" do
    # (()) and ()() both result in floor 0
    assert calculate_floor("(())") == 0
    assert calculate_floor("()()") == 0

    # ((( and (()(()( both result in floor 3
    assert calculate_floor("(((") == 3
    assert calculate_floor("(()(()(") == 3

    # ))((((( also results in floor 3
    assert calculate_floor("))((((( ") == 3

    # ()) and ))( both result in floor -1 (the first basement level)
    assert calculate_floor("())") == -1
    assert calculate_floor("))(") == -1

    # ))) and )())()) both result in floor -3
    assert calculate_floor(")))") == -3
    assert calculate_floor(")())())") == -3
  end

  test "empty string" do
    # Empty string should result in floor 0
    assert calculate_floor("") == 0
  end

  test "single parenthesis" do
    # Single open parenthesis should be floor 1
    assert calculate_floor("(") == 1

    # Single close parenthesis should be floor -1
    assert calculate_floor(")") == -1
  end

  test "examples from problem part b" do
    # ) causes him to enter the basement at character position 1
    assert find_basement_position(")") == 1

    # ()()) causes him to enter the basement at character position 5
    assert find_basement_position("()())") == 5
  end

  test "never enter basement" do
    # Test cases where Santa never enters the basement
    assert find_basement_position("(") == -1  # Stays at floor 1
    assert find_basement_position("(((") == -1  # Stays positive
    assert find_basement_position("(()") == -1  # Ends at floor 1
    assert find_basement_position("") == -1  # Empty string
  end

  test "immediate basement entry" do
    # Direct entry to basement
    assert find_basement_position(")") == 1
    assert find_basement_position("))") == 1  # First ) takes him to -1
  end

  test "later basement entry" do
    # Later entry to basement
    # ()()) causes him to enter the basement at character position 5
    assert find_basement_position("()())") == 5
    # A longer example: (((())))) -> floors 1,2,3,4,3,2,1,0,-1 at position 9
    assert find_basement_position("(((()))))") == 9
  end
end
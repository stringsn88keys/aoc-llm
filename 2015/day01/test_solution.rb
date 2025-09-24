require_relative 'solution'

require 'minitest/autorun'

class TestSolution < Minitest::Test
  def test_examples_from_problem_part_a
    # (()) and ()() both result in floor 0
    assert_equal 0, calculate_floor('(())')
    assert_equal 0, calculate_floor('()()')
    
    # ((( and (()(()( both result in floor 3
    assert_equal 3, calculate_floor('(((')
    assert_equal 3, calculate_floor('(()(()(')
    
    # ))((((( also results in floor 3
    assert_equal 3, calculate_floor('))((((( ')
    
    # ()) and ))( both result in floor -1 (the first basement level)
    assert_equal(-1, calculate_floor('())'))
    assert_equal(-1, calculate_floor('))('))
    
    # ))) and )())()) both result in floor -3
    assert_equal(-3, calculate_floor(')))'))
    assert_equal(-3, calculate_floor(')())())'))
  end
  
  def test_empty_string
    # Empty string should result in floor 0
    assert_equal 0, calculate_floor('')
  end
  
  def test_single_parenthesis
    # Single open parenthesis should be floor 1
    assert_equal 1, calculate_floor('(')
    
    # Single close parenthesis should be floor -1
    assert_equal(-1, calculate_floor(')'))
  end
end

class TestPartB < Minitest::Test
  def test_examples_from_problem_part_b
    # ) causes him to enter the basement at character position 1
    assert_equal 1, find_basement_position(')')
    
    # ()()) causes him to enter the basement at character position 5
    assert_equal 5, find_basement_position('()())')
  end
  
  def test_never_enter_basement
    # Test cases where Santa never enters the basement
    assert_equal(-1, find_basement_position('('))  # Stays at floor 1
    assert_equal(-1, find_basement_position('((('))  # Stays positive
    assert_equal(-1, find_basement_position('(()'))  # Ends at floor 1
    assert_equal(-1, find_basement_position(''))  # Empty string
  end
  
  def test_immediate_basement_entry
    # Direct entry to basement
    assert_equal 1, find_basement_position(')')
    assert_equal 1, find_basement_position('))')  # First ) takes him to -1
  end
  
  def test_later_basement_entry
    # Later entry to basement
    # ()()) causes him to enter the basement at character position 5
    assert_equal 5, find_basement_position('()())')
    # A longer example: (((())))) -> floors 1,2,3,4,3,2,1,0,-1 at position 9
    assert_equal 9, find_basement_position('(((()))))')
  end
end
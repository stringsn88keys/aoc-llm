#!/usr/bin/env ruby
# Tests for Advent of Code 2015 Day 2 Part A

require 'test/unit'
require_relative 'solution'

class TestDay2PartA < Test::Unit::TestCase
  
  def test_calculate_wrapping_paper_examples
    # Example 1: 2x3x4 should need 58 square feet
    assert_equal(58, calculate_wrapping_paper(2, 3, 4))
    
    # Example 2: 1x1x10 should need 43 square feet
    assert_equal(43, calculate_wrapping_paper(1, 1, 10))
  end
  
  def test_calculate_wrapping_paper_edge_cases
    # All sides equal (cube)
    # Surface area: 2*4 + 2*4 + 2*4 = 24, smallest side: 4, total: 28
    assert_equal(28, calculate_wrapping_paper(2, 2, 2))
    
    # Very small box
    # Surface area: 2*1 + 2*1 + 2*1 = 6, smallest side: 1, total: 7
    assert_equal(7, calculate_wrapping_paper(1, 1, 1))
  end
  
  def test_parse_dimensions
    assert_equal([2, 3, 4], parse_dimensions("2x3x4"))
    assert_equal([1, 1, 10], parse_dimensions("1x1x10"))
    assert_equal([100, 200, 300], parse_dimensions("100x200x300"))
  end
  
  def test_solve_part_a_with_test_input
    # Test input should give 58 + 43 = 101
    result = solve_part_a('../data/input-test-a.txt')
    assert_equal(101, result)
  end
end
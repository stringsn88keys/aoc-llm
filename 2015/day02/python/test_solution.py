#!/usr/bin/env python3
"""
Tests for Advent of Code 2015 Day 2 Part A
"""

import unittest
import sys
import os

# Add the parent directory to sys.path so we can import solution
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from solution import calculate_wrapping_paper, parse_dimensions, solve_part_a

class TestDay2PartA(unittest.TestCase):
    
    def test_calculate_wrapping_paper_examples(self):
        """Test the examples from the problem description."""
        # Example 1: 2x3x4 should need 58 square feet
        paper = calculate_wrapping_paper(2, 3, 4)
        self.assertEqual(paper, 58)
        
        # Example 2: 1x1x10 should need 43 square feet
        paper = calculate_wrapping_paper(1, 1, 10)
        self.assertEqual(paper, 43)
    
    def test_calculate_wrapping_paper_edge_cases(self):
        """Test edge cases."""
        # All sides equal (cube)
        paper = calculate_wrapping_paper(2, 2, 2)
        # Surface area: 2*4 + 2*4 + 2*4 = 24, smallest side: 4, total: 28
        self.assertEqual(paper, 28)
        
        # Very thin box
        paper = calculate_wrapping_paper(1, 1, 1)
        # Surface area: 2*1 + 2*1 + 2*1 = 6, smallest side: 1, total: 7
        self.assertEqual(paper, 7)
    
    def test_parse_dimensions(self):
        """Test parsing dimension strings."""
        self.assertEqual(parse_dimensions("2x3x4"), (2, 3, 4))
        self.assertEqual(parse_dimensions("1x1x10"), (1, 1, 10))
        self.assertEqual(parse_dimensions("100x200x300"), (100, 200, 300))
    
    def test_solve_part_a_with_test_input(self):
        """Test with the provided test input."""
        # Test input should give 58 + 43 = 101
        result = solve_part_a('../data/input-test-a.txt')
        self.assertEqual(result, 101)

if __name__ == '__main__':
    unittest.main()
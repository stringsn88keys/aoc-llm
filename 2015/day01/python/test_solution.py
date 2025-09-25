import unittest
from solution import calculate_floor


class TestSolution(unittest.TestCase):
    
    def test_examples_from_problem(self):
        # (()) and ()() both result in floor 0
        self.assertEqual(calculate_floor('(())'), 0)
        self.assertEqual(calculate_floor('()()'), 0)
        
        # ((( and (()(()( both result in floor 3
        self.assertEqual(calculate_floor('((('), 3)
        self.assertEqual(calculate_floor('(()(()('), 3)
        
        # ))((((( also results in floor 3
        self.assertEqual(calculate_floor('))((((('), 3)
        
        # ()) and ))( both result in floor -1 (the first basement level)
        self.assertEqual(calculate_floor('())'), -1)
        self.assertEqual(calculate_floor('))('), -1)
        
        # ))) and )())()) both result in floor -3
        self.assertEqual(calculate_floor(')))'), -3)
        self.assertEqual(calculate_floor(')())())'), -3)
    
    def test_empty_string(self):
        # Empty string should result in floor 0
        self.assertEqual(calculate_floor(''), 0)
    
    def test_single_parenthesis(self):
        # Single open parenthesis should be floor 1
        self.assertEqual(calculate_floor('('), 1)
        
        # Single close parenthesis should be floor -1
        self.assertEqual(calculate_floor(')'), -1)
    
    def test_from_test_input_file(self):
        # Test with the content from input-test-a.txt
        with open('../data/input-test-a.txt', 'r') as file:
            test_input = file.read().strip()
        # input-test-a.txt contains '(())' which should result in floor 0
        self.assertEqual(calculate_floor(test_input), 0)


if __name__ == '__main__':
    unittest.main()
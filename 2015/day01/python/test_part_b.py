import unittest
from solution import find_basement_position


class TestPartBSolution(unittest.TestCase):
    
    def test_examples_from_problem(self):
        # ) causes him to enter the basement at character position 1
        self.assertEqual(find_basement_position(')'), 1)
        
        # ()()) causes him to enter the basement at character position 5
        self.assertEqual(find_basement_position('()())'), 5)
    
    def test_never_enter_basement(self):
        # Test cases where Santa never enters the basement
        self.assertEqual(find_basement_position('('), -1)  # Stays at floor 1
        self.assertEqual(find_basement_position('((('), -1)  # Stays positive
        self.assertEqual(find_basement_position('(()'), -1)  # Ends at floor 1
        self.assertEqual(find_basement_position(''), -1)  # Empty string
    
    def test_immediate_basement_entry(self):
        # Direct entry to basement
        self.assertEqual(find_basement_position(')'), 1)
        self.assertEqual(find_basement_position('))'), 1)  # First ) takes him to -1
    
    def test_later_basement_entry(self):
        # Later entry to basement
        # ()()) causes him to enter the basement at character position 5
        self.assertEqual(find_basement_position('()())'), 5)
        # A longer example to ensure our function works: (((())))) -> floors 1,2,3,4,3,2,1,0,-1 at position 9
        self.assertEqual(find_basement_position('(((()))))'), 9)


if __name__ == '__main__':
    unittest.main()
import sys
from solution import find_basement_position


def main():
    """
    Main function for Part B to find the position where Santa first enters the basement.
    """
    # Use input.txt by default, or take from command-line argument
    input_file = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    
    try:
        with open(input_file, 'r') as file:
            instructions = file.read().strip()
        
        # Find position where Santa first enters basement
        basement_pos = find_basement_position(instructions)
        if basement_pos != -1:
            print(f"The first character that causes Santa to enter the basement is at position {basement_pos}")
        else:
            print("Santa never enters the basement")
    except FileNotFoundError:
        print(f"{input_file} not found. Please create the file with the puzzle input.")
        print("Example usage: find_basement_position('()())') returns", find_basement_position('()())'))


if __name__ == "__main__":
    main()
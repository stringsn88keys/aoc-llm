import sys


def calculate_floor(instructions):
    """
    Calculate the final floor based on the instructions.
    
    Args:
        instructions (str): String containing '(' and ')' characters
        
    Returns:
        int: The final floor number
    """
    floor = 0
    for char in instructions:
        if char == '(':
            floor += 1
        elif char == ')':
            floor -= 1
    return floor


def find_basement_position(instructions):
    """
    Find the position of the first character that causes Santa to enter the basement (floor -1).
    
    Args:
        instructions (str): String containing '(' and ')' characters
        
    Returns:
        int: The position (1-indexed) of the character that causes Santa to first enter the basement,
             or -1 if he never enters the basement
    """
    floor = 0
    for i, char in enumerate(instructions):
        if char == '(':
            floor += 1
        elif char == ')':
            floor -= 1
            
        if floor == -1:
            return i + 1  # Position is 1-indexed
            
    return -1  # Never entered the basement


def main():
    """
    Main function to read input from file and calculate the result.
    """
    # Use input.txt by default, or take from command-line argument
    input_file = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    
    try:
        with open(input_file, 'r') as file:
            instructions = file.read().strip()
        
        # Part A: Calculate final floor
        final_floor = calculate_floor(instructions)
        print(f"Santa ends up on floor {final_floor}")
        
        # Part B: Find position where Santa first enters basement
        basement_pos = find_basement_position(instructions)
        if basement_pos != -1:
            print(f"The first character that causes Santa to enter the basement is at position {basement_pos}")
        else:
            print("Santa never enters the basement")
    except FileNotFoundError:
        print(f"{input_file} not found. Please create the file with the puzzle input.")
        print("Example usage: calculate_floor('(((') returns", calculate_floor('((('))
        print("Example usage: find_basement_position('()())') returns", find_basement_position('()())'))


if __name__ == "__main__":
    main()
# Calculate the final floor based on instructions
def calculate_floor(instructions)
  floor = 0
  instructions.each_char do |char|
    if char == '('
      floor += 1
    elsif char == ')'
      floor -= 1
    end
  end
  floor
end

# Find the position of the first character that causes Santa to enter the basement (floor -1)
def find_basement_position(instructions)
  floor = 0
  instructions.each_char.with_index(1) do |char, index|
    if char == '('
      floor += 1
    elsif char == ')'
      floor -= 1
    end
    
    return index if floor == -1
  end
  -1 # Never entered the basement
end

# Main function that handles file input and outputs results
def main
  input_file = ARGV[0]? || "input.txt"
  
  begin
    instructions = File.read(input_file).strip
    
    # Part A: Calculate final floor
    final_floor = calculate_floor(instructions)
    puts "Santa ends up on floor #{final_floor}"
    
    # Part B: Find position where Santa first enters basement
    basement_pos = find_basement_position(instructions)
    if basement_pos != -1
      puts "The first character that causes Santa to enter the basement is at position #{basement_pos}"
    else
      puts "Santa never enters the basement"
    end
  rescue File::NotFoundError
    puts "#{input_file} not found. Please create the file with the puzzle input."
    puts "Example usage: calculate_floor('(((') returns #{calculate_floor("(((")}"
    puts "Example usage: find_basement_position('()())') returns #{find_basement_position("()())")}"
  end
end

main
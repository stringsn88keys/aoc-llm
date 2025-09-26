# Advent of Code 2015 Day 2 - I Was Told There Would Be No Math

def calculate_wrapping_paper(l : Int32, w : Int32, h : Int32) : Int32
  # Surface area: 2*l*w + 2*w*h + 2*h*l
  surface_area = 2*l*w + 2*w*h + 2*h*l
  
  # Extra paper: area of smallest side
  sides = [l*w, w*h, h*l]
  smallest_side = sides.min
  
  surface_area + smallest_side
end

def parse_dimensions(line : String) : Array(Int32)
  # Parse a line like "2x3x4" into dimensions [2, 3, 4]
  line.strip.split('x').map(&.to_i)
end

def solve_part_a(filename : String) : Int32
  total_paper = 0
  
  File.each_line(filename) do |line|
    next if line.strip.empty?
    
    dimensions = parse_dimensions(line)
    l, w, h = dimensions[0], dimensions[1], dimensions[2]
    paper_needed = calculate_wrapping_paper(l, w, h)
    total_paper += paper_needed
  end
  
  total_paper
end

def main
  # Determine input file
  filename = ARGV.size > 0 ? ARGV[0] : "../data/input.txt"
  
  begin
    total_paper = solve_part_a(filename)
    puts "The elves should order #{total_paper} square feet of wrapping paper"
  rescue File::NotFoundError
    puts "Error: File '#{filename}' not found."
    exit(1)
  end
end

main
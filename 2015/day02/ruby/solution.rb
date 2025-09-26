#!/usr/bin/env ruby
# Advent of Code 2015 Day 2 - I Was Told There Would Be No Math

def calculate_wrapping_paper(l, w, h)
  # Surface area: 2*l*w + 2*w*h + 2*h*l
  surface_area = 2*l*w + 2*w*h + 2*h*l
  
  # Extra paper: area of smallest side
  sides = [l*w, w*h, h*l]
  smallest_side = sides.min
  
  surface_area + smallest_side
end

def parse_dimensions(line)
  # Parse a line like '2x3x4' into dimensions [2, 3, 4]
  line.strip.split('x').map(&:to_i)
end

def solve_part_a(filename)
  total_paper = 0
  
  File.readlines(filename).each do |line|
    next if line.strip.empty?
    
    l, w, h = parse_dimensions(line)
    paper_needed = calculate_wrapping_paper(l, w, h)
    total_paper += paper_needed
  end
  
  total_paper
rescue Errno::ENOENT
  puts "Error: File '#{filename}' not found."
  exit(1)
end

def main
  # Determine input file
  filename = ARGV.length > 0 ? ARGV[0] : '../data/input.txt'
  
  total_paper = solve_part_a(filename)
  puts "The elves should order #{total_paper} square feet of wrapping paper"
end

if __FILE__ == $0
  main
end
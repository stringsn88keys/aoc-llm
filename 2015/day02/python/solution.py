#!/usr/bin/env python3
"""
Advent of Code 2015 Day 2 - I Was Told There Would Be No Math
"""

import sys
import os

def calculate_wrapping_paper(l, w, h):
    """Calculate wrapping paper needed for a present with dimensions l x w x h."""
    # Surface area: 2*l*w + 2*w*h + 2*h*l
    surface_area = 2*l*w + 2*w*h + 2*h*l
    
    # Extra paper: area of smallest side
    sides = [l*w, w*h, h*l]
    smallest_side = min(sides)
    
    return surface_area + smallest_side

def parse_dimensions(line):
    """Parse a line like '2x3x4' into dimensions (2, 3, 4)."""
    parts = line.strip().split('x')
    return tuple(map(int, parts))

def solve_part_a(filename):
    """Solve part A: calculate total wrapping paper needed."""
    total_paper = 0
    
    try:
        with open(filename, 'r') as f:
            for line in f:
                if line.strip():
                    l, w, h = parse_dimensions(line)
                    paper_needed = calculate_wrapping_paper(l, w, h)
                    total_paper += paper_needed
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)
    
    return total_paper

def main():
    # Determine input file
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = '../data/input.txt'
    
    total_paper = solve_part_a(filename)
    print(f"The elves should order {total_paper} square feet of wrapping paper")

if __name__ == "__main__":
    main()
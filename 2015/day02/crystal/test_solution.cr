require "spec"
require "./solution"

describe "Day 2 Part A" do
  describe "calculate_wrapping_paper" do
    it "works with examples" do
      calculate_wrapping_paper(2, 3, 4).should eq(58)
      calculate_wrapping_paper(1, 1, 10).should eq(43)
    end
    
    it "works with edge cases" do
      calculate_wrapping_paper(2, 2, 2).should eq(28)  # Cube
      calculate_wrapping_paper(1, 1, 1).should eq(7)   # Unit cube
    end
  end
  
  describe "parse_dimensions" do
    it "parses dimension strings correctly" do
      parse_dimensions("2x3x4").should eq([2, 3, 4])
      parse_dimensions("1x1x10").should eq([1, 1, 10])
      parse_dimensions("100x200x300").should eq([100, 200, 300])
    end
  end
  
  describe "solve_part_a" do
    it "works with test input" do
      solve_part_a("../data/input-test-a.txt").should eq(101)
    end
  end
end
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

describe "Day 2 Part B" do
  describe "calculate_ribbon" do
    it "works with examples from instructions" do
      # 2x3x4: wrapping = 2+2+3+3 = 10, bow = 2*3*4 = 24, total = 34
      calculate_ribbon(2, 3, 4).should eq(34)
      # 1x1x10: wrapping = 1+1+1+1 = 4, bow = 1*1*10 = 10, total = 14
      calculate_ribbon(1, 1, 10).should eq(14)
    end
    
    it "works with edge cases" do
      # 2x2x2: wrapping = 2+2+2+2 = 8, bow = 2*2*2 = 8, total = 16
      calculate_ribbon(2, 2, 2).should eq(16)  # Cube
      # 1x1x1: wrapping = 1+1+1+1 = 4, bow = 1*1*1 = 1, total = 5
      calculate_ribbon(1, 1, 1).should eq(5)   # Unit cube
    end
  end
  
  describe "solve_part_b" do
    it "works with test input" do
      # Test input: 2x3x4 and 1x1x10
      # 2x3x4: 34 feet of ribbon
      # 1x1x10: 14 feet of ribbon
      # Total: 34 + 14 = 48
      solve_part_b("../data/input-test-a.txt").should eq(48)
    end
  end
end
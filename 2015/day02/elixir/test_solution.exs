ExUnit.start()

Code.require_file("solution.exs", __DIR__)

defmodule Day2Test do
  use ExUnit.Case

  test "calculate_wrapping_paper with examples" do
    assert Day2.calculate_wrapping_paper(2, 3, 4) == 58
    assert Day2.calculate_wrapping_paper(1, 1, 10) == 43
  end

  test "calculate_wrapping_paper with edge cases" do
    assert Day2.calculate_wrapping_paper(2, 2, 2) == 28  # Cube
    assert Day2.calculate_wrapping_paper(1, 1, 1) == 7   # Unit cube
  end

  test "parse_dimensions" do
    assert Day2.parse_dimensions("2x3x4") == {2, 3, 4}
    assert Day2.parse_dimensions("1x1x10") == {1, 1, 10}
    assert Day2.parse_dimensions("100x200x300") == {100, 200, 300}
  end

  test "solve_part_a with test input" do
    assert Day2.solve_part_a("../data/input-test-a.txt") == 101
  end

  test "calculate_ribbon with examples from instructions" do
    # 2x3x4: wrapping = 2+2+3+3 = 10, bow = 2*3*4 = 24, total = 34
    assert Day2.calculate_ribbon(2, 3, 4) == 34
    # 1x1x10: wrapping = 1+1+1+1 = 4, bow = 1*1*10 = 10, total = 14
    assert Day2.calculate_ribbon(1, 1, 10) == 14
  end

  test "calculate_ribbon with edge cases" do
    # 2x2x2: wrapping = 2+2+2+2 = 8, bow = 2*2*2 = 8, total = 16
    assert Day2.calculate_ribbon(2, 2, 2) == 16  # Cube
    # 1x1x1: wrapping = 1+1+1+1 = 4, bow = 1*1*1 = 1, total = 5
    assert Day2.calculate_ribbon(1, 1, 1) == 5   # Unit cube
  end

  test "solve_part_b with test input" do
    # Test input: 2x3x4 and 1x1x10
    # 2x3x4: 34 feet of ribbon
    # 1x1x10: 14 feet of ribbon
    # Total: 34 + 14 = 48
    assert Day2.solve_part_b("../data/input-test-a.txt") == 48
  end
end

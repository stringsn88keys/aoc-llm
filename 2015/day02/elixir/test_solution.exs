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
end

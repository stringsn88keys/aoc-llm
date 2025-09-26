# Advent of Code 2015 Day 2 - I Was Told There Would Be No Math

defmodule Day2 do
  def calculate_wrapping_paper(l, w, h) do
    # Surface area: 2*l*w + 2*w*h + 2*h*l
    surface_area = 2*l*w + 2*w*h + 2*h*l

    # Extra paper: area of smallest side
    sides = [l*w, w*h, h*l]
    smallest_side = Enum.min(sides)

    surface_area + smallest_side
  end

  def calculate_ribbon(l, w, h) do
    # Ribbon for wrapping: shortest distance around sides (smallest perimeter)
    perimeters = [2*(l+w), 2*(w+h), 2*(h+l)]
    wrapping_ribbon = Enum.min(perimeters)

    # Ribbon for bow: volume of the present
    bow_ribbon = l * w * h

    wrapping_ribbon + bow_ribbon
  end

  def parse_dimensions(line) do
    # Parse a line like "2x3x4" into dimensions {2, 3, 4}
    line
    |> String.trim()
    |> String.split("x")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  def solve_part_a(filename) do
    filename
    |> File.read!()
    |> String.split("\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_dimensions/1)
    |> Enum.map(fn {l, w, h} -> calculate_wrapping_paper(l, w, h) end)
    |> Enum.sum()
  end

  def solve_part_b(filename) do
    filename
    |> File.read!()
    |> String.split("\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_dimensions/1)
    |> Enum.map(fn {l, w, h} -> calculate_ribbon(l, w, h) end)
    |> Enum.sum()
  end

  def main do
    # Determine input file
    filename = case System.argv() do
      [file] -> file
      [] -> "../data/input.txt"
      _ -> "../data/input.txt"
    end

    try do
      total_paper = solve_part_a(filename)
      total_ribbon = solve_part_b(filename)
      IO.puts("The elves should order #{total_paper} square feet of wrapping paper")
      IO.puts("The elves should order #{total_ribbon} feet of ribbon")
    rescue
      File.Error ->
        IO.puts("Error: File '#{filename}' not found.")
        System.halt(1)
    end
  end
end

Day2.main()

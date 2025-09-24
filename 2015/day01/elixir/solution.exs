defmodule Solution do
  def calculate_floor(instructions) do
    instructions
    |> String.graphemes()
    |> Enum.reduce(0, fn char, acc ->
      case char do
        "(" -> acc + 1
        ")" -> acc - 1
        _ -> acc
      end
    end)
  end

  def find_basement_position(instructions) do
    instructions
    |> String.graphemes()
    |> Enum.with_index(1)
    |> find_basement_helper(0)
  end

  defp find_basement_helper([], _floor), do: -1

  defp find_basement_helper([{char, index} | rest], floor) do
    new_floor = case char do
      "(" -> floor + 1
      ")" -> floor - 1
      _ -> floor
    end

    if new_floor == -1 do
      index
    else
      find_basement_helper(rest, new_floor)
    end
  end

  def main(args) do
    input_file = case args do
      [] -> "input.txt"
      [file] -> file
    end

    case File.read(input_file) do
      {:ok, content} ->
        instructions = String.trim(content)

        # Part A: Calculate final floor
        final_floor = calculate_floor(instructions)
        IO.puts("Santa ends up on floor #{final_floor}")

        # Part B: Find position where Santa first enters basement
        basement_pos = find_basement_position(instructions)
        if basement_pos != -1 do
          IO.puts("The first character that causes Santa to enter the basement is at position #{basement_pos}")
        else
          IO.puts("Santa never enters the basement")
        end

      {:error, _reason} ->
        IO.puts("#{input_file} not found. Please create the file with the puzzle input.")
        IO.puts("Example usage: calculate_floor('(((') returns #{calculate_floor('(((')}")
        IO.puts("Example usage: find_basement_position('()())') returns #{find_basement_position('()())')}")
    end
  end
end

# Run main function if this file is executed directly
if :filename.basename(__ENV__.file) == "solution.exs" do
  Solution.main(System.argv())
end
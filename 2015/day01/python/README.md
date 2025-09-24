# Day 1: Not Quite Lisp

## Problem Description
Santa needs to find the correct floor in a large apartment building. He starts on the ground floor (floor 0) and follows instructions:
- `(` means go up one floor
- `)` means go down one floor

### Part A
Determine which floor Santa ends up on after following all the instructions.

### Part B
Find the position of the first character that causes Santa to enter the basement (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.

## Solution
This solution includes implementations in multiple languages:

### Python
- `solution.py`: Main script to calculate both the final floor (Part A) and the position where Santa first enters the basement (Part B)
- `part_b_solution.py`: Separate script specifically for Part B solution
- `test_solution.py`: Unit tests for Part A
- `test_part_b.py`: Unit tests for Part B

### Ruby
- `solution.rb`: Implementation in Ruby for both parts A and B
- `test_solution.rb`: Unit tests using MiniTest framework

### Elixir
- `solution.exs`: Implementation in Elixir for both parts A and B
- `test_solution.exs`: Unit tests using ExUnit framework

### Erlang
- `solution.erl`: Implementation in Erlang for both parts A and B
- `test_solution.erl`: Unit tests using EUnit framework

### Go
- `solution.go`: Implementation in Go for both parts A and B
- `test_solution_test.go`: Unit tests using Go's testing package

### Crystal
- `solution.cr`: Implementation in Crystal for both parts A and B
- `test_solution.cr`: Tests for Crystal implementation

## Input Files
- `input.txt`: Default file for puzzle input
- `input-test-a.txt`: Sample test input for Part A
- `input-full.txt`: Full puzzle input file
- `test_input_basement1.txt`, `test_input_basement2.txt`: Test files for Part B examples

## How to run
### Python
1. Run `python solution.py` to get both Part A and Part B answers
2. Run `python solution.py input-file.txt` to use a specific input file
3. Run `python part_b_solution.py` for just Part B solution
4. Run `python -m unittest test_solution.py` to run Part A tests
5. Run `python -m unittest test_part_b.py` to run Part B tests

### Ruby
1. Run `ruby solution.rb` to get both Part A and Part B answers
2. Run `ruby solution.rb input-file.txt` to use a specific input file
3. Run `ruby test_solution.rb` to run tests

### Elixir
1. Run `elixir solution.exs` to get both Part A and Part B answers
2. Run `elixir solution.exs input-file.txt` to use a specific input file
3. Run `elixir -r solution.exs test_solution.exs` to run tests

### Erlang
1. Compile with `erlc solution.erl` 
2. Run with `erl -noshell -s santa_floor main "input.txt" -s init stop`
3. Run tests with `erl -noshell -s santa_floor_tests test -s init stop`

### Go
1. First run `go mod init solution`
2. Run `go run solution.go` to get both Part A and Part B answers
3. Run `go run solution.go input-file.txt` to use a specific input file
4. Run `go test` to run tests

### Crystal
1. Install Crystal from https://crystal-lang.org/
2. Run `crystal solution.cr` to get both Part A and Part B answers
3. Run `crystal solution.cr input-file.txt` to use a specific input file
4. Run `crystal test_solution.cr` to run the tests

## Examples
As per the problem:
### Part A
- `(())` and `()()` both result in floor 0
- `(((` and `(()(()(` both result in floor 3
- `))((((( ` also results in floor 3
- `())` and `))( ` both result in floor -1
- `)))` and `)())())` both result in floor -3

### Part B
- `)` causes him to enter the basement at character position 1
- `()())` causes him to enter the basement at character position 5
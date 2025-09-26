# Advent of Code Test Runner

This repository contains solutions for Advent of Code problems implemented in multiple programming languages, along with a comprehensive test runner that validates all solutions and ensures consistency across languages.

## Structure

```
aoc-llm/
├── run_tests.py          # Main test runner (Python)
├── run_tests.sh          # Shell wrapper script
├── README.md             # This file
└── YYYY/                 # Year directories (e.g., 2015/)
    └── dayNN/            # Day directories (e.g., day01/)
        ├── data/         # Input and test data files
        │   ├── input.txt
        │   ├── input-full.txt
        │   └── test_*.txt
        ├── part-a-instructions.txt
        ├── part-b-instructions.txt
        └── LANGUAGE/     # Language-specific implementations
            ├── solution.EXT
            └── test_solution.EXT
```

## Supported Languages

The test runner supports the following programming languages:

### Core Languages
- **Python** (`.py`) - Uses `python3` and unittest framework
- **Go** (`.go`) - Uses `go test` and `go run`
- **Ruby** (`.rb`) - Uses `ruby` interpreter
- **Crystal** (`.cr`) - Uses `crystal run`
- **Elixir** (`.exs`) - Uses `elixir` interpreter
- **Erlang** (`.erl`) - Uses `erlc` compiler and `erl` runtime

### Extended Languages
- **Haskell** (`.hs`) - Uses `ghc` compiler and `runhaskell`
- **Lua** (`.lua`) - Uses `lua` interpreter
- **Forth** (`.fs`) - Uses `gforth` interpreter
- **Rust** (`.rs`) - Uses `rustc` compiler and `cargo test`
- **APL** (`.apl`) - Uses GNU-APL `apl` interpreter

## Usage

### Quick Start

Run the comprehensive test suite:

```bash
./run_tests.sh
```

Or run directly with Python:

```bash
python3 run_tests.py
```

### What the Test Runner Does

1. **Discovers** all year/day/language combinations automatically
2. **Runs test suites** for each language implementation
3. **Executes solutions** with real input data
4. **Compares outputs** across different language implementations
5. **Reports inconsistencies** if solutions produce different results
6. **Generates a summary table** showing pass/fail status for all tests

### Output Format

The test runner provides three types of output:

#### 1. Real-time Progress
```
Testing 2015/day01:
  python... ✓
  go... ✓
  ruby... ✗
  crystal... ✓
```

#### 2. Summary Table
```
Year/Day     Language   Tests    Part A                    Part B                    Consistent
2015/day01   crystal    PASS     Santa ends up on floor 74  The first character th... ✓
2015/day01   elixir     PASS     Santa ends up on floor 74  The first character th... 
2015/day01   go         PASS     Santa ends up on floor 74  The first character th... 
2015/day01   python     PASS     Santa ends up on floor 74  The first character th... 
```

#### 3. Detailed Output Comparison
```
2015/day01:
  Part A (CONSISTENT):
    crystal: Santa ends up on floor 74
    go: Santa ends up on floor 74
    python: Santa ends up on floor 74
  Part B (CONSISTENT):
    crystal: The first character that causes Santa to enter the basement is at position 1795
    go: The first character that causes Santa to enter the basement is at position 1795
    python: The first character that causes Santa to enter the basement is at position 1795
```

## Requirements

### Core Requirements
- **Python 3.6+** - Required for the test runner itself

### Language-Specific Requirements
Install any languages you want to test:

#### Core Languages
- **Python**: Usually pre-installed on macOS/Linux
- **Go**: Download from [golang.org](https://golang.org/)
- **Ruby**: Usually pre-installed on macOS, or install via package manager
- **Crystal**: Install from [crystal-lang.org](https://crystal-lang.org/)
- **Elixir**: Install from [elixir-lang.org](https://elixir-lang.org/)
- **Erlang**: Install from [erlang.org](https://www.erlang.org/) or via package manager

#### Extended Languages
- **Haskell**: Install GHC from [haskell.org](https://www.haskell.org/)
- **Lua**: Install from [lua.org](https://www.lua.org/) or via package manager
- **Forth**: Install Gforth from [gforth.org](https://gforth.org/) or via package manager
- **Rust**: Install from [rust-lang.org](https://www.rust-lang.org/)
- **APL**: Install GNU-APL via package manager (e.g., `brew install gnu-apl`)

### Installation Examples

**macOS (using Homebrew):**
```bash
# Core languages
brew install go ruby crystal elixir erlang

# Extended languages  
brew install ghc lua gforth rust gnu-apl
```

**Ubuntu/Debian:**
```bash
# Core languages
sudo apt-get install golang-go ruby crystal elixir erlang

# Extended languages
sudo apt-get install ghc lua5.3 gforth rustc cargo gnu-apl
```

## File Conventions

For the test runner to work correctly, follow these conventions:

### Solution Files
- **Main solution**: `solution.{ext}` (e.g., `solution.py`, `solution.go`)
- **Test files**: `test_solution.{ext}` or `test*.{ext}`

### Input Files (in `data/` directory)
- **Main input**: `input.txt` or `input-full.txt`
- **Test inputs**: `input-test-a.txt`, `test_input_*.txt`

### Solution Output Format
Solutions should accept input file paths as command-line arguments and reference data files using `../data/filename.txt`. Output results to stdout in this format:
```
Santa ends up on floor 74
The first character that causes Santa to enter the basement is at position 1795
```
- **Line 1**: Part A result  
- **Line 2**: Part B result (if applicable)

**Example usage:**
```bash
cd 2015/day01/python
python3 solution.py ../data/input.txt
```

### Test Framework Requirements
- **Python**: Use `unittest` framework
- **Go**: Use built-in `testing` package
- **Crystal**: Use `spec` framework
- **Elixir**: Use `ExUnit` framework
- **Erlang**: Use custom test runner
- **Haskell**: Use `HUnit` or custom test framework
- **Lua**: Use custom test framework
- **Forth**: Use custom test assertions
- **Rust**: Use built-in `#[test]` and `cargo test`
- **APL**: Use custom test framework

## Features

### Automatic Discovery
The test runner automatically finds all implemented solutions without manual configuration.

### Language Isolation
Each language implementation runs in isolation, referencing shared input files from the `../data/` directory using relative paths.

### Timeout Protection
All test executions have a 30-second timeout to prevent hanging.

### Cleanup
Temporary files (compiled binaries) are automatically cleaned up after each test. Input files remain in the `data/` directory and are referenced using relative paths.

### Cross-Language Validation
The most powerful feature is consistency checking - if you implement the same problem in multiple languages, the test runner will verify they all produce identical results.

## Troubleshooting

### Common Issues

1. **Language not found**: Install the missing language runtime
2. **Test failures**: Check that test files follow the expected format
3. **Inconsistent outputs**: Review solution logic across languages
4. **File not found errors**: Ensure input files are in the `data/` directory

### Debugging Individual Languages

Run tests for specific languages manually:

```bash
# Python
cd 2015/day01/python
python3 -m unittest test_solution.py

# Go
cd 2015/day01/go  
go test -v

# Ruby
cd 2015/day01/ruby
ruby test_solution.rb
```

## Contributing

When adding new solutions:

1. Follow the established directory structure
2. Implement both solution and test files
3. Ensure output format matches the expected pattern
4. Run the test suite to verify consistency with existing implementations

The test runner makes it easy to maintain quality across multiple language implementations and catch regressions early.
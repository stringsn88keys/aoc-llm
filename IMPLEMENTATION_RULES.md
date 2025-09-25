# Advent of Code Implementation Rules

This document defines the standards and requirements for implementing Advent of Code solution#### Lua
- **Framework**: Custom test framework
- **Execution**: `lua test_solution.lua`
- **Success**: Exit code 0, all tests pass

#### Forth (Gforth)
- **Framework**: Custom test assertions
- **Execution**: `gforth test_solution.fs`
- **Success**: Exit code 0, all assertions passmultiple programming languages.

## Supported Languages

### Core Languages (Required)
- **Python** - Primary reference implementation
- **Go** - Systems programming perspective
- **Ruby** - Dynamic scripting language
- **Elixir** - Functional/concurrent programming
- **Erlang** - Actor model and fault tolerance
- **Crystal** - Compiled Ruby-like syntax

### Extended Languages (Optional)
- **Haskell** - Pure functional programming
- **Lua** - Lightweight scripting
- **Forth** - Stack-based programming with Gforth

## Directory Structure

Each solution must follow this exact structure:

```
YYYY/dayNN/
├── data/                           # Shared input data
│   ├── input.txt                   # Main puzzle input
│   ├── input-full.txt             # Alternative/full input
│   ├── input-test-a.txt           # Part A test data
│   └── test_input_*.txt           # Additional test cases
├── part-a-instructions.txt         # Problem description Part A
├── part-b-instructions.txt         # Problem description Part B
└── LANGUAGE/                       # Language-specific implementation
    ├── solution.EXT                # Main solution file
    └── test_solution.EXT           # Test file
```

## File Naming Conventions

### Solution Files
- **Python**: `solution.py`
- **Go**: `solution.go` (with `go.mod`)
- **Ruby**: `solution.rb`
- **Crystal**: `solution.cr`
- **Elixir**: `solution.exs`
- **Erlang**: `solution.erl`
- **Haskell**: `solution.hs`
- **Lua**: `solution.lua`
- **Forth**: `solution.fs`

### Test Files
- **Python**: `test_solution.py`
- **Go**: `test_solution_test.go`
- **Ruby**: `test_solution.rb`
- **Crystal**: `test_solution.cr`
- **Elixir**: `test_solution.exs`
- **Erlang**: `test_solution.erl`
- **Haskell**: `test_solution.hs`
- **Lua**: `test_solution.lua`
- **Forth**: `test_solution.fs`

## Input File Handling

### Requirements
1. **Relative Path Access**: All solutions must access input files using `../data/filename.txt`
2. **Command Line Arguments**: Solutions must accept input file path as first argument
3. **Default Fallback**: If no argument provided, default to `../data/input.txt`

### Example Usage
```bash
cd YYYY/dayNN/LANGUAGE
LANGUAGE_CMD solution.EXT ../data/input-full.txt
```

## Output Format Standards

### Standard Output Format
All solutions must output results to stdout in this exact format:

```
Part A result description: VALUE
Part B result description: VALUE
```

### Examples
```
Santa ends up on floor 74
The first character that causes Santa to enter the basement is at position 1795
```

### Requirements
- **Line 1**: Part A result with descriptive text
- **Line 2**: Part B result with descriptive text (if applicable)
- **No extra output**: Avoid debug prints, timing info, etc.
- **Consistent format**: Use same description text across all languages

## Testing Standards

### Test Framework Requirements

#### Python
- **Framework**: `unittest`
- **Execution**: `python3 test_solution.py`
- **Success**: Exit code 0, no test failures

#### Go  
- **Framework**: Built-in `testing` package
- **Execution**: `go test -v`
- **Success**: All tests pass, exit code 0

#### Ruby
- **Framework**: Built-in `Test::Unit` or custom assertions
- **Execution**: `ruby test_solution.rb`
- **Success**: Exit code 0, all assertions pass

#### Crystal
- **Framework**: Built-in `spec` or custom assertions
- **Execution**: `crystal run test_solution.cr`
- **Success**: Exit code 0, all tests pass

#### Elixir
- **Framework**: `ExUnit`
- **Execution**: `elixir test_solution.exs`
- **Success**: All tests pass, no failures

#### Erlang
- **Framework**: `EUnit`
- **Execution**: `erlc test_solution.erl && erl -noshell -s test_solution test -s init stop`
- **Success**: All tests pass, exit code 0

#### Haskell
- **Framework**: `HUnit` or `QuickCheck`
- **Execution**: `runhaskell test_solution.hs` or `ghc test_solution.hs && ./test_solution`
- **Success**: All tests pass, exit code 0

#### Lua
- **Framework**: `luaunit` or custom assertions
- **Execution**: `lua test_solution.lua`
- **Success**: Exit code 0, all tests pass

#### APL (GNU-APL)
- **Framework**: Custom test assertions
- **Execution**: `apl -f test_solution.apl` or `apl < test_solution.apl`
- **Success**: All assertions pass, proper output

### Test Coverage Requirements

Each test file must include:

1. **Example Tests**: All examples from problem description
2. **Edge Cases**: Empty input, single characters, boundary conditions
3. **Part A Tests**: Comprehensive coverage of Part A logic
4. **Part B Tests**: Comprehensive coverage of Part B logic (if applicable)
5. **Input File Tests**: At least one test using actual test data files

## Language-Specific Requirements

### Python
- **Version**: Python 3.6+
- **Style**: Follow PEP 8
- **Modules**: Standard library only (unless specified)
- **Error Handling**: Proper file I/O error handling

### Go
- **Version**: Go 1.18+
- **Modules**: Use `go.mod` with appropriate Go version
- **Style**: Follow `gofmt` standards
- **Error Handling**: Proper error handling patterns

### Ruby
- **Version**: Ruby 2.7+
- **Style**: Follow Ruby style guide
- **Gems**: Standard library only (unless specified)

### Crystal
- **Version**: Crystal 1.0+
- **Style**: Follow Crystal conventions
- **Shards**: Standard library only (unless specified)

### Elixir
- **Version**: Elixir 1.12+
- **Style**: Follow Elixir style guide
- **Dependencies**: Standard library only
- **Module Loading**: Proper `Code.require_file` usage

### Erlang
- **Version**: Erlang/OTP 24+
- **Style**: Follow Erlang conventions
- **Module Naming**: Module name must match filename

### Haskell
- **Version**: GHC 8.10+
- **Style**: Follow Haskell style guide
- **Extensions**: Minimize language extensions
- **Dependencies**: Base libraries preferred

### Lua
- **Version**: Lua 5.3+
- **Style**: Follow Lua style conventions
- **Libraries**: Standard library only

### Forth (Gforth)
- **Version**: Gforth 0.7+
- **Style**: Standard Forth conventions
- **Words**: Use standard and Gforth-specific words

## Quality Standards

### Code Quality
1. **Readability**: Clear, well-commented code
2. **Efficiency**: Reasonable time/space complexity
3. **Maintainability**: Modular, well-structured functions
4. **Consistency**: Similar structure across languages

### Testing Quality
1. **Coverage**: All major code paths tested
2. **Assertions**: Clear, descriptive test assertions
3. **Independence**: Tests don't depend on external state
4. **Repeatability**: Tests produce consistent results

### Cross-Language Consistency
1. **Identical Results**: All implementations must produce identical output
2. **Same Algorithm**: Similar algorithmic approach when reasonable
3. **Error Handling**: Consistent error messages and handling
4. **Performance**: Reasonable performance characteristics

## Validation Process

### Automated Testing
The test runner performs these validations:

1. **Environment Check**: Verify language runtime exists
2. **Compilation**: Ensure code compiles (for compiled languages)
3. **Unit Tests**: Run all test suites
4. **Solution Execution**: Run with real input data
5. **Output Comparison**: Verify identical results across languages
6. **Consistency Report**: Flag any discrepancies

### Manual Review
Before accepting new implementations:

1. **Code Review**: Ensure code follows language conventions
2. **Test Review**: Verify comprehensive test coverage
3. **Documentation**: Check that code is properly commented
4. **Performance**: Ensure reasonable execution time

## Runtime Environment Requirements

### System Dependencies
The test runner checks for these language runtimes:

- `python3` - Python interpreter
- `go` - Go compiler and runtime
- `ruby` - Ruby interpreter
- `crystal` - Crystal compiler
- `elixir` - Elixir interpreter
- `erl`, `erlc` - Erlang compiler and runtime
- `ghc`, `runhaskell` - Glasgow Haskell Compiler
- `lua` - Lua interpreter
- `gforth` - Gforth interpreter

### Installation Verification
Missing runtimes are reported but don't fail the test suite. This allows:
- Partial implementation across available languages
- Easy identification of missing dependencies
- Graceful degradation when some languages unavailable

## Best Practices

### Implementation Strategy
1. **Start with Python**: Use as reference implementation
2. **Test Early**: Write tests alongside solution code
3. **Compare Output**: Regularly verify consistency across languages
4. **Document Edge Cases**: Note any language-specific considerations

### Development Workflow
1. **Read Problem**: Understand requirements thoroughly
2. **Design Algorithm**: Plan approach before coding
3. **Implement & Test**: Code and test in parallel
4. **Cross-Validate**: Run test runner to verify consistency
5. **Document**: Add comments and update any special considerations

### Performance Considerations
- **Reasonable Complexity**: Don't over-optimize unless necessary
- **Language Idiomatic**: Use each language's strengths
- **Timeout Limits**: Solutions should complete within 30 seconds
- **Memory Usage**: Be mindful of memory consumption for large inputs

## Error Handling Standards

### File I/O Errors
- Graceful handling of missing input files
- Clear error messages with file paths
- Appropriate exit codes

### Input Validation
- Handle malformed input gracefully
- Provide meaningful error messages
- Don't crash on unexpected input formats

### Test Failures
- Clear assertion messages
- Helpful debugging information
- Proper test isolation

This document ensures consistency, quality, and maintainability across all Advent of Code implementations while supporting the diverse strengths of different programming languages.
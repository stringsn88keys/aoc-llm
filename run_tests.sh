#!/bin/bash
# 
# Advent of Code Test Runner Shell Script
# 
# This script provides an easy way to run the comprehensive test suite
# for all Advent of Code solutions across multiple languages.
#

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

echo -e "${BOLD}${BLUE}Advent of Code Test Suite Runner${NC}"
echo "======================================"

# Check if Python 3 is available
if ! command -v python3 &> /dev/null; then
    echo -e "${RED}Error: Python 3 is required but not installed.${NC}"
    exit 1
fi

# Check for optional language runtimes
echo "Checking available language runtimes..."

check_lang() {
    if command -v "$2" &> /dev/null; then
        echo -e "  ${GREEN}✓${NC} $1 ($2)"
    else
        echo -e "  ${YELLOW}✗${NC} $1 ($2 not found)"
    fi
}

check_lang "python" "python3"
check_lang "go" "go"
check_lang "ruby" "ruby"
check_lang "crystal" "crystal"
check_lang "elixir" "elixir"
check_lang "erlang" "erl"
check_lang "haskell" "ghc"
check_lang "lua" "lua"
check_lang "apl" "apl"

echo ""
echo "Running comprehensive test suite..."
echo "This will:"
echo "  1. Run all test suites for each language"
echo "  2. Execute full solutions with real input"
echo "  3. Compare outputs across languages for consistency"
echo "  4. Generate a detailed report"
echo ""

# Run the Python test runner
python3 run_tests.py

exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo -e "\n${GREEN}${BOLD}All tests completed successfully!${NC}"
else
    echo -e "\n${RED}${BOLD}Test run completed with errors.${NC}"
fi

exit $exit_code
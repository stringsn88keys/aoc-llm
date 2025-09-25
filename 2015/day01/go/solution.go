package main

import (
	"fmt"
	"os"
	"strings"
)

// calculateFloor calculates the final floor based on instructions
func calculateFloor(instructions string) int {
	floor := 0
	for _, char := range instructions {
		if char == '(' {
			floor++
		} else if char == ')' {
			floor--
		}
	}
	return floor
}

// findBasementPosition finds the position of the first character that causes Santa to enter the basement (floor -1)
func findBasementPosition(instructions string) int {
	floor := 0
	for i, char := range instructions {
		if char == '(' {
			floor++
		} else if char == ')' {
			floor--
		}

		if floor == -1 {
			return i + 1 // Position is 1-indexed
		}
	}
	return -1 // Never entered the basement
}

func main() {
	// Use input.txt by default, or take from command-line argument
	inputFile := "input.txt"
	if len(os.Args) > 1 {
		inputFile = os.Args[1]
	}

	// Read the entire file content
	content, err := os.ReadFile(inputFile)
	if err != nil {
		fmt.Printf("%s not found. Please create the file with the puzzle input.\\n", inputFile)
		fmt.Printf("Example usage: calculateFloor(\"(((\") returns %d\\n", calculateFloor("((("))
		fmt.Printf("Example usage: findBasementPosition(\"()())\") returns %d\\n", findBasementPosition("()())"))
		return
	}

	instructions := strings.TrimSpace(string(content))

	// Part A: Calculate final floor
	finalFloor := calculateFloor(instructions)
	fmt.Printf("Santa ends up on floor %d\n", finalFloor)

	// Part B: Find position where Santa first enters basement
	basementPos := findBasementPosition(instructions)
	if basementPos != -1 {
		fmt.Printf("The first character that causes Santa to enter the basement is at position %d\n", basementPos)
	} else {
		fmt.Println("Santa never enters the basement")
	}
}

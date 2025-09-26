package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// calculateWrappingPaper calculates the wrapping paper needed for a present
func calculateWrappingPaper(l, w, h int) int {
	// Surface area: 2*l*w + 2*w*h + 2*h*l
	surfaceArea := 2*l*w + 2*w*h + 2*h*l

	// Extra paper: area of smallest side
	sides := []int{l * w, w * h, h * l}
	smallestSide := sides[0]
	for _, side := range sides[1:] {
		if side < smallestSide {
			smallestSide = side
		}
	}

	return surfaceArea + smallestSide
}

// parseDimensions parses a line like "2x3x4" into dimensions
func parseDimensions(line string) (int, int, int, error) {
	parts := strings.Split(strings.TrimSpace(line), "x")
	if len(parts) != 3 {
		return 0, 0, 0, fmt.Errorf("invalid dimension format: %s", line)
	}

	l, err := strconv.Atoi(parts[0])
	if err != nil {
		return 0, 0, 0, err
	}

	w, err := strconv.Atoi(parts[1])
	if err != nil {
		return 0, 0, 0, err
	}

	h, err := strconv.Atoi(parts[2])
	if err != nil {
		return 0, 0, 0, err
	}

	return l, w, h, nil
}

// solvePartA solves part A: calculate total wrapping paper needed
func solvePartA(filename string) (int, error) {
	file, err := os.Open(filename)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	totalPaper := 0
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" {
			continue
		}

		l, w, h, err := parseDimensions(line)
		if err != nil {
			return 0, err
		}

		paperNeeded := calculateWrappingPaper(l, w, h)
		totalPaper += paperNeeded
	}

	if err := scanner.Err(); err != nil {
		return 0, err
	}

	return totalPaper, nil
}

func main() {
	// Determine input file
	filename := "../data/input.txt"
	if len(os.Args) > 1 {
		filename = os.Args[1]
	}

	totalPaper, err := solvePartA(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("The elves should order %d square feet of wrapping paper\n", totalPaper)
}

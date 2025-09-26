package main

import (
	"testing"
)

func TestCalculateWrappingPaper(t *testing.T) {
	tests := []struct {
		l, w, h  int
		expected int
	}{
		{2, 3, 4, 58}, // Example 1: 2*6 + 2*12 + 2*8 + 6 = 52 + 6 = 58
		{1, 1, 10, 43}, // Example 2: 2*1 + 2*10 + 2*10 + 1 = 42 + 1 = 43
		{2, 2, 2, 28},  // Cube: 2*4 + 2*4 + 2*4 + 4 = 24 + 4 = 28
		{1, 1, 1, 7},   // Unit cube: 2*1 + 2*1 + 2*1 + 1 = 6 + 1 = 7
	}
	
	for _, test := range tests {
		result := calculateWrappingPaper(test.l, test.w, test.h)
		if result != test.expected {
			t.Errorf("calculateWrappingPaper(%d, %d, %d) = %d, want %d", 
				test.l, test.w, test.h, result, test.expected)
		}
	}
}

func TestParseDimensions(t *testing.T) {
	tests := []struct {
		input    string
		l, w, h  int
		hasError bool
	}{
		{"2x3x4", 2, 3, 4, false},
		{"1x1x10", 1, 1, 10, false},
		{"100x200x300", 100, 200, 300, false},
		{"invalid", 0, 0, 0, true},
		{"1x2", 0, 0, 0, true},
		{"1x2x3x4", 0, 0, 0, true},
	}
	
	for _, test := range tests {
		l, w, h, err := parseDimensions(test.input)
		if test.hasError {
			if err == nil {
				t.Errorf("parseDimensions(%s) expected error, got nil", test.input)
			}
		} else {
			if err != nil {
				t.Errorf("parseDimensions(%s) unexpected error: %v", test.input, err)
			}
			if l != test.l || w != test.w || h != test.h {
				t.Errorf("parseDimensions(%s) = (%d, %d, %d), want (%d, %d, %d)",
					test.input, l, w, h, test.l, test.w, test.h)
			}
		}
	}
}

func TestSolvePartAWithTestInput(t *testing.T) {
	result, err := solvePartA("../data/input-test-a.txt")
	if err != nil {
		t.Fatalf("solvePartA error: %v", err)
	}
	
	expected := 101 // 58 + 43
	if result != expected {
		t.Errorf("solvePartA = %d, want %d", result, expected)
	}
}
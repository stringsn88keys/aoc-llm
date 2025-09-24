package main

import (
	"testing"
)

func TestExamplesFromProblemPartA(t *testing.T) {
	// (()) and ()() both result in floor 0
	if calculateFloor("(())") != 0 {
		t.Errorf("Expected (()) to result in floor 0, got %d", calculateFloor("(())"))
	}
	if calculateFloor("()()") != 0 {
		t.Errorf("Expected ()() to result in floor 0, got %d", calculateFloor("()()"))
	}

	// ((( and (()(()( both result in floor 3
	if calculateFloor("(((") != 3 {
		t.Errorf("Expected ((( to result in floor 3, got %d", calculateFloor("((("))
	}
	if calculateFloor("(()(()(") != 3 {
		t.Errorf("Expected (()(()( to result in floor 3, got %d", calculateFloor("(()(()("))
	}

	// ))((((( also results in floor 3
	if calculateFloor("))((((( ") != 3 {
		t.Errorf("Expected ))(((((  to result in floor 3, got %d", calculateFloor("))((((( "))
	}

	// ()) and ))( both result in floor -1 (the first basement level)
	if calculateFloor("())") != -1 {
		t.Errorf("Expected ()) to result in floor -1, got %d", calculateFloor("())"))
	}
	if calculateFloor("))(") != -1 {
		t.Errorf("Expected ))( to result in floor -1, got %d", calculateFloor("))("))
	}

	// ))) and )())()) both result in floor -3
	if calculateFloor(")))") != -3 {
		t.Errorf("Expected ))) to result in floor -3, got %d", calculateFloor(")))"))
	}
	if calculateFloor(")())())") != -3 {
		t.Errorf("Expected )())()) to result in floor -3, got %d", calculateFloor(")())())"))
	}
}

func TestEmptyString(t *testing.T) {
	// Empty string should result in floor 0
	if calculateFloor("") != 0 {
		t.Errorf("Expected empty string to result in floor 0, got %d", calculateFloor(""))
	}
}

func TestSingleParenthesis(t *testing.T) {
	// Single open parenthesis should be floor 1
	if calculateFloor("(") != 1 {
		t.Errorf("Expected ( to result in floor 1, got %d", calculateFloor("("))
	}

	// Single close parenthesis should be floor -1
	if calculateFloor(")") != -1 {
		t.Errorf("Expected ) to result in floor -1, got %d", calculateFloor(")"))
	}
}

func TestExamplesFromProblemPartB(t *testing.T) {
	// ) causes him to enter the basement at character position 1
	if findBasementPosition(")") != 1 {
		t.Errorf("Expected ) to cause basement entry at position 1, got %d", findBasementPosition(")"))
	}

	// ()()) causes him to enter the basement at character position 5
	if findBasementPosition("()())") != 5 {
		t.Errorf("Expected ()()) to cause basement entry at position 5, got %d", findBasementPosition("()())"))
	}
}

func TestNeverEnterBasement(t *testing.T) {
	// Test cases where Santa never enters the basement
	if findBasementPosition("(") != -1 { // Stays at floor 1
		t.Errorf("Expected ( not to enter basement, got position %d", findBasementPosition("("))
	}
	if findBasementPosition("(((") != -1 { // Stays positive
		t.Errorf("Expected ((( not to enter basement, got position %d", findBasementPosition("((("))
	}
	if findBasementPosition("(()") != -1 { // Ends at floor 1
		t.Errorf("Expected (() not to enter basement, got position %d", findBasementPosition("(()"))
	}
	if findBasementPosition("") != -1 { // Empty string
		t.Errorf("Expected empty string not to enter basement, got position %d", findBasementPosition(""))
	}
}

func TestImmediateBasementEntry(t *testing.T) {
	// Direct entry to basement
	if findBasementPosition(")") != 1 {
		t.Errorf("Expected ) to cause basement entry at position 1, got %d", findBasementPosition(")"))
	}
	if findBasementPosition("))") != 1 { // First ) takes him to -1
		t.Errorf("Expected )) to cause basement entry at position 1, got %d", findBasementPosition("))"))
	}
}

func TestLaterBasementEntry(t *testing.T) {
	// Later entry to basement
	// ()()) causes him to enter the basement at character position 5
	if findBasementPosition("()())") != 5 {
		t.Errorf("Expected ()()) to cause basement entry at position 5, got %d", findBasementPosition("()())"))
	}
	// A longer example: (((())))) -> floors 1,2,3,4,3,2,1,0,-1 at position 9
	if findBasementPosition("(((()))))") != 9 {
		t.Errorf("Expected (((())))) to cause basement entry at position 9, got %d", findBasementPosition("(((()))))"))
	}
}

package main

import (
	"fmt"
	"time"
)

func ValidBraces(str string) bool {
	var stack []rune

	for _, char := range str {
		switch char {
		case '(', '[', '{':
			stack = append(stack, char)
		case ')', ']', '}':
			if len(stack) == 0 {
				return false // Stack is empty, no matching opening brace
			}

			// Check if the last element in the stack matches the closing brace
			last := stack[len(stack)-1]
			stack = stack[:len(stack)-1] // Pop from stack

			if (char == ')' && last != '(') || (char == ']' && last != '[') || (char == '}' && last != '{') {
				return false
			}
		}
	}

	return len(stack) == 0 // If stack is empty, all braces were matched
}

func main() {
	start := time.Now()
	elapsed := time.Since(start)

	fmt.Println(ValidBraces("(){}[]"))   // True
	fmt.Println(ValidBraces("([{}])"))   // True
	fmt.Println(ValidBraces("(}"))       // False
	fmt.Println(ValidBraces("[(])"))     // False
	fmt.Println(ValidBraces("[({})](]")) // False

	fmt.Println("Execution Time:", elapsed)
}

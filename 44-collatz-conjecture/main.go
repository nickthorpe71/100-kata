package main

import "fmt"

// More efficient for computing just the count
func CollatzSimple(n int) int {
	count := 1
	for n != 1 {
		if n%2 == 0 {
			n /= 2
		} else {
			n = 3*n + 1
		}
		count++
	}
	return count
}

func Collatz(n int) int {
	// Create a memoization map
	memo := make(map[int][]int)

	sequence := CollatzR(n, memo)
	return len(sequence)
}

func CollatzR(n int, memo map[int][]int) []int {
	// Check if the result for n is already in the memo
	if val, exists := memo[n]; exists {
		return val
	}

	// Base case: if n is 1, return [1]
	if n == 1 {
		memo[n] = []int{1}
		return memo[n]
	}

	// Recursive case: compute the next number in the sequence
	var next int
	if n%2 == 0 {
		next = n / 2
	} else {
		next = 3*n + 1
	}

	// Compute the sequence recursively and store it in the memo
	result := append([]int{n}, CollatzR(next, memo)...)
	memo[n] = result // Store in memo for future use
	return result
}

func main() {
	fmt.Println(Collatz(20))               // 8)
	fmt.Println(Collatz(15))               // 18)
	fmt.Println(Collatz(100))              // 26)
	fmt.Println(Collatz(10))               // 7)
	fmt.Println(Collatz(500))              // 111)
	fmt.Println(Collatz(1))                // 1)
	fmt.Println(Collatz(1000000000))       // 101)
	fmt.Println(Collatz(1000000000000000)) // 276)
}

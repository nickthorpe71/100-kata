// Improved Solutions for Pair Finder Problem
// Multiple approaches with explanations

use std::collections::HashMap;

// ============================================================================
// YOUR ORIGINAL SOLUTION (with bug analysis)
// ============================================================================
fn find_pair_original(numbers: Vec<i32>, target: i32) -> Vec<usize> {
    let mut value_index_map: HashMap<i32, usize> = HashMap::new();

    // BUG: This loop overwrites indices for duplicate values!
    // Example: [3, 3] -> map will only have {3: 1} not {3: 0}
    for (index, &value) in numbers.iter().enumerate() {
        value_index_map.insert(value, index); // Later duplicates overwrite
    }

    for (index, &value) in numbers.iter().enumerate() {
        let other_half = target - value;
        if let Some(&other_index) = value_index_map.get(&other_half) {
            if other_index == index {
                continue; // Good: avoids using same element twice
            }
            return vec![index, other_index];
        }
    }

    vec![0, 1] // Bad: returns fake indices instead of proper error handling
}

// ============================================================================
// IMPROVEMENT 1: SINGLE-PASS SOLUTION (Optimal)
// ============================================================================
/// Time: O(n), Space: O(n)
/// This is the classic interview solution - build map as you search
fn find_pair_single_pass(numbers: Vec<i32>, target: i32) -> Vec<usize> {
    let mut seen: HashMap<i32, usize> = HashMap::new();
    
    for (index, &value) in numbers.iter().enumerate() {
        let complement = target - value;
        
        // Check if we've seen the complement before
        if let Some(&complement_index) = seen.get(&complement) {
            return vec![complement_index, index]; // Found it!
        }
        
        // Haven't found it yet, store current value for future
        seen.insert(value, index);
    }
    
    // Problem guarantees solution exists, but good to handle
    panic!("No valid pair found - problem constraints violated")
}

// ============================================================================
// IMPROVEMENT 2: FUNCTIONAL R* STYLE (Most Idiomatic)
// ============================================================================
/// Using iterator combinators and find_map for functional approach
fn find_pair_functional(numbers: Vec<i32>, target: i32) -> Vec<usize> {
    let mut seen = HashMap::new();
    
    numbers
        .iter()
        .enumerate()
        .find_map(|(index, &value)| {
            let complement = target - value;
            
            // Try to find complement, or store current value
            seen.get(&complement)
                .map(|&complement_index| vec![complement_index, index])
                .or_else(|| {
                    seen.insert(value, index);
                    None // Continue searching
                })
        })
        .expect("No valid pair found")
}

// ============================================================================
// IMPROVEMENT 3: CLEAREST VERSION (Best for interviews)
// ============================================================================
/// Most readable version with explicit variable names and comments
fn find_pair_interview_best(numbers: Vec<i32>, target: i32) -> Vec<usize> {
    let mut value_to_index = HashMap::with_capacity(numbers.len());
    
    for (current_index, &current_value) in numbers.iter().enumerate() {
        let needed_value = target - current_value;
        
        // Have we seen the value we need?
        match value_to_index.get(&needed_value) {
            Some(&previous_index) => {
                // Found a valid pair!
                return vec![previous_index, current_index];
            }
            None => {
                // Store this value for future lookups
                value_to_index.insert(current_value, current_index);
            }
        }
    }
    
    unreachable!("Problem guarantees exactly one solution exists")
}

// ============================================================================
// IMPROVEMENT 4: HANDLING EDGE CASES EXPLICITLY
// ============================================================================
/// Version that validates input and handles all edge cases
fn find_pair_production(numbers: Vec<i32>, target: i32) -> Result<Vec<usize>, &'static str> {
    // Input validation
    if numbers.len() < 2 {
        return Err("Need at least 2 numbers");
    }
    
    let mut seen = HashMap::new();
    
    for (index, &value) in numbers.iter().enumerate() {
        // Check for overflow when calculating complement
        let complement = match target.checked_sub(value) {
            Some(c) => c,
            None => continue, // Skip if overflow
        };
        
        if let Some(&complement_index) = seen.get(&complement) {
            return Ok(vec![complement_index, index]);
        }
        
        seen.insert(value, index);
    }
    
    Err("No two numbers sum to target")
}

// ============================================================================
// ALTERNATIVE APPROACH: SORTED ARRAY TWO-POINTER
// ============================================================================
/// If we're allowed to sort first, two-pointer approach works
/// Time: O(n log n), Space: O(n) for index tracking
fn find_pair_two_pointer(numbers: Vec<i32>, target: i32) -> Vec<usize> {
    // Create pairs of (value, original_index) and sort by value
    let mut indexed: Vec<(i32, usize)> = numbers
        .iter()
        .enumerate()
        .map(|(i, &v)| (v, i))
        .collect();
    
    indexed.sort_by_key(|&(value, _)| value);
    
    let mut left = 0;
    let mut right = indexed.len() - 1;
    
    while left < right {
        let sum = indexed[left].0 + indexed[right].0;
        
        match sum.cmp(&target) {
            std::cmp::Ordering::Equal => {
                // Found it! Return original indices
                let mut result = vec![indexed[left].1, indexed[right].1];
                result.sort(); // Ensure lower index first
                return result;
            }
            std::cmp::Ordering::Less => left += 1,
            std::cmp::Ordering::Greater => right -= 1,
        }
    }
    
    panic!("No valid pair found")
}

// ============================================================================
// TESTS TO VERIFY ALL IMPLEMENTATIONS
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_duplicate_values_bug() {
        // This case fails with the original implementation!
        let numbers = vec![3, 3];
        let target = 6;
        
        // Original fails because second 3 overwrites first in map
        // let result = find_pair_original(numbers.clone(), target);
        
        // All improved versions handle this correctly
        assert_eq!(find_pair_single_pass(numbers.clone(), target), vec![0, 1]);
        assert_eq!(find_pair_functional(numbers.clone(), target), vec![0, 1]);
        assert_eq!(find_pair_interview_best(numbers.clone(), target), vec![0, 1]);
    }
    
    #[test]
    fn test_standard_case() {
        let numbers = vec![2, 7, 11, 15];
        let target = 9;
        
        assert_eq!(find_pair_single_pass(numbers.clone(), target), vec![0, 1]);
        assert_eq!(find_pair_functional(numbers.clone(), target), vec![0, 1]);
        assert_eq!(find_pair_interview_best(numbers.clone(), target), vec![0, 1]);
    }
    
    #[test]
    fn test_negative_numbers() {
        let numbers = vec![-3, 4, 3, 90];
        let target = 0;
        
        assert_eq!(find_pair_single_pass(numbers.clone(), target), vec![0, 2]);
    }
}

// ============================================================================
// INTERVIEW FOLLOW-UP QUESTIONS & ANSWERS
// ============================================================================

/*
Q1: "What if we need to find ALL pairs that sum to target?"

fn find_all_pairs(numbers: Vec<i32>, target: i32) -> Vec<Vec<usize>> {
    let mut seen = HashMap::new();
    let mut results = Vec::new();
    
    for (index, &value) in numbers.iter().enumerate() {
        let complement = target - value;
        
        // Check all indices where complement appears
        if let Some(indices) = seen.get(&complement) {
            for &prev_index in indices {
                results.push(vec![prev_index, index]);
            }
        }
        
        // Store in a Vec to handle duplicates
        seen.entry(value).or_insert_with(Vec::new).push(index);
    }
    
    results
}

Q2: "What if the array is already sorted?"
A: Use two-pointer approach (see find_pair_two_pointer above)
   Time: O(n), Space: O(1) - better space complexity!

Q3: "What if we can use the same element twice?"
A: Remove the index equality check, but be careful with duplicates

Q4: "Can you do it without extra space?"
A: Only if array is sorted (two-pointer) or we can modify input (sort in-place)
   Otherwise, need O(n) space for the HashMap

Q5: "What about integer overflow?"
A: Use checked_sub() as shown in find_pair_production

Q6: "How would you test this thoroughly?"
A: Test cases should include:
   - Basic examples
   - Duplicates in array
   - Negative numbers
   - Zero as target
   - Large numbers (near i32::MAX)
   - Array with only 2 elements
   - Target that requires first and last elements
*/

// ============================================================================
// COMPLEXITY ANALYSIS
// ============================================================================

/*
Time Complexity:
- HashMap approach: O(n) - single pass through array
- Two-pointer (sorted): O(n log n) - dominated by sorting
- Brute force: O(n²) - check all pairs

Space Complexity:
- HashMap approach: O(n) - store up to n elements
- Two-pointer (sorted): O(1) if we can modify input, O(n) otherwise
- Brute force: O(1) - no extra storage

Trade-offs:
- HashMap: Best time, uses extra space
- Two-pointer: No extra space if sorted, but slower if need to sort
- Brute force: No extra space but slow - only for tiny arrays

For interviews, HashMap single-pass is usually the expected solution.
*/
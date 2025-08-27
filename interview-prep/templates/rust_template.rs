// R* Style Rust Template for Interview Problems
// Remember: No external crates, functional style, explicit naming

use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    // Test cases
    run_tests();
}

fn solve_problem(input: Vec<i32>) -> i32 {
    // Your solution here
    // Follow R* principles:
    // - Prefer iterators over loops when clear
    // - Use descriptive variable names
    // - Minimize mutability
    // - Be explicit, not clever
    
    unimplemented!("Add your solution")
}

fn run_tests() {
    // Test case 1
    let input1 = vec![1, 2, 3];
    let expected1 = 6;
    let result1 = solve_problem(input1.clone());
    assert_eq!(result1, expected1, "Test 1 failed: input {:?}", input1);
    println!("✓ Test 1 passed");
    
    // Test case 2
    let input2 = vec![4, 5, 6];
    let expected2 = 15;
    let result2 = solve_problem(input2.clone());
    assert_eq!(result2, expected2, "Test 2 failed: input {:?}", input2);
    println!("✓ Test 2 passed");
    
    // Edge case - empty input
    // let input3 = vec![];
    // let expected3 = 0;
    // let result3 = solve_problem(input3.clone());
    // assert_eq!(result3, expected3, "Test 3 failed: input {:?}", input3);
    // println!("✓ Test 3 passed");
    
    println!("\n✅ All tests passed!");
}

// Helper functions following R* naming conventions
fn calculate_sum(numbers: &[i32]) -> i32 {
    numbers.iter().sum()
}

fn find_maximum(numbers: &[i32]) -> Option<i32> {
    numbers.iter().copied().max()
}

fn find_minimum(numbers: &[i32]) -> Option<i32> {
    numbers.iter().copied().min()
}

// Common patterns in R* style

// Pattern: Two-pointer technique
fn two_pointer_example(sorted_array: &[i32], target: i32) -> Option<(usize, usize)> {
    let mut left = 0;
    let mut right = sorted_array.len() - 1;
    
    while left < right {
        let sum = sorted_array[left] + sorted_array[right];
        match sum.cmp(&target) {
            std::cmp::Ordering::Equal => return Some((left, right)),
            std::cmp::Ordering::Less => left += 1,
            std::cmp::Ordering::Greater => right -= 1,
        }
    }
    None
}

// Pattern: Sliding window
fn sliding_window_example(array: &[i32], window_size: usize) -> Vec<i32> {
    array
        .windows(window_size)
        .map(|window| window.iter().sum())
        .collect()
}

// Pattern: Frequency counting
fn count_frequencies(items: &[i32]) -> HashMap<i32, usize> {
    items.iter().fold(HashMap::new(), |mut map, &item| {
        *map.entry(item).or_insert(0) += 1;
        map
    })
}

// Pattern: Graph traversal setup
#[allow(dead_code)]
struct Graph {
    adjacency_list: HashMap<i32, Vec<i32>>,
}

#[allow(dead_code)]
impl Graph {
    fn new() -> Self {
        Graph {
            adjacency_list: HashMap::new(),
        }
    }
    
    fn add_edge(&mut self, from: i32, to: i32) {
        self.adjacency_list
            .entry(from)
            .or_insert_with(Vec::new)
            .push(to);
    }
}
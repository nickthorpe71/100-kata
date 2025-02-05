use std::time::Instant;

fn decompose(n: i64) -> Option<Vec<i64>> {
    fn helper(remaining: i64, max: i64) -> Option<Vec<i64>> {
        if remaining == 0 {
            return Some(vec![]);
        }

        for i in (1..max).rev() {
            let square = i * i;
            if square <= remaining {
                if let Some(mut result) = helper(remaining - square, i) {
                    result.push(i);
                    return Some(result);
                }
            }
        }

        None
    }

    helper(n * n, n)
}

fn decompose_b(n: i64) -> Option<Vec<i64>> {
    partition(n * n, n)
}

fn partition(sqrt: i64, n: i64) -> Option<Vec<i64>> {
    if sqrt < 0 { return None; }
    if sqrt == 0 { return Some(Vec::new()); }

    for i in (0..n).rev() {
        let part = partition(sqrt - i * i, i);
        if part != None { return Some([part.unwrap(), vec![i]].concat()); }
    }

    None
}

fn decompose_c(n: i64) -> Option<Vec<i64>> {
    let mut result = [0; 100];  // Fixed-size array with a large enough bound
    if partition_c(n * n, n, &mut result, 0).is_some() {
        // Collect non-zero elements and reverse them
        let collected: Vec<i64> = result.iter().filter(|&&x| x > 0).copied().collect();
        Some(collected.into_iter().rev().collect())
    } else {
        None
    }
}

fn partition_c(sqrt: i64, n: i64, result: &mut [i64; 100], index: usize) -> Option<()> {
    if sqrt == 0 {
        return Some(());  // Found a valid decomposition
    }

    for i in (1..n).rev() {
        let square = i * i;
        if square <= sqrt {
            result[index] = i;  // Place current candidate in the result array
            if partition_c(sqrt - square, i, result, index + 1).is_some() {
                return Some(());  // Valid path found
            }
            result[index] = 0;  // Backtrack (reset the current position)
        }
    }

    None  // No valid solution found
}

fn decompose_d(n: i64) -> Option<Vec<i64>> {
    let target = n * n;
    let mut stack: Vec<(i64, i64, Vec<i64>)> = vec![(target, n, Vec::new())];

    while let Some((remaining, max, mut path)) = stack.pop() {
        if remaining == 0 {
            path.reverse();
            return Some(path);  // Found a valid decomposition
        }

        // Try all candidates from max - 1 down to 1
        for i in (1..max).rev() {
            let square = i * i;
            if square <= remaining {
                let mut new_path = path.clone();
                new_path.push(i);
                stack.push((remaining - square, i, new_path));
            }
        }
    }

    None  // No valid solution found
}


fn main() {
    let n = 1992105479;  // Large value for benchmarking

    // Benchmark for decompose (Version A)
    let start = Instant::now();
    let result_a = decompose(n);
    let duration_a = start.elapsed();
    println!("decompose: Result = {:?}, Time = {:?}", result_a, duration_a);

    // Benchmark for decompose_b (Version B)
    let start = Instant::now();
    let result_b = decompose_b(n);
    let duration_b = start.elapsed();
    println!("decompose_b: Result = {:?}, Time = {:?}", result_b, duration_b);

    // Benchmark for decompose_c (Version C)
    let start = Instant::now();
    let result_c = decompose_c(n);
    let duration_c = start.elapsed();
    println!("decompose_c: Result = {:?}, Time = {:?}", result_c, duration_c);

    // Benchmark for decompose_d (Version D)
    let start = Instant::now();
    let result_d = decompose_d(n);
    let duration_d = start.elapsed();
    println!("decompose_d: Result = {:?}, Time = {:?}", result_d, duration_d);
}

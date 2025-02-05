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

fn main() {
    println!("Decompose 11: {:?}", decompose(11)); // Expected: Some([1, 2, 4, 10])
    println!("Decompose 50: {:?}", decompose(50)); // Expected: Some([1, 3, 5, 8, 49])
    println!("Decompose 4: {:?}", decompose(4));   // Expected: None (since no valid decomposition exists)
}
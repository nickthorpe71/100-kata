fn longest_slide_down(pyramid: &[Vec<u16>]) -> u16 {
    fn find_max_path(
        pyramid: &[Vec<u16>],
        row: usize,
        col: usize,
        cache: &mut Vec<Vec<Option<u16>>>
    ) -> u16 {
        // Check if we have reached the bottom of the pyramid
        if row == pyramid.len() {
            return 0;
        }

        // Check if we have already calculated the value for this cell
        if let Some(value) = cache[row][col] {
            return value;
        }

        let left = find_max_path(pyramid, row + 1, col, cache);
        let right = find_max_path(pyramid, row + 1, col + 1, cache);
        let max = left.max(right) + pyramid[row][col];
        cache[row][col] = Some(max);
        max
    }

    let mut cache = pyramid
        .iter()
        .map(|row| vec![None; row.len()])
        .collect();
    find_max_path(pyramid, 0, 0, &mut cache)
}

fn longest_slide_down_dp(pyramid: &[Vec<u16>]) -> u16 {
    // Start with the bottom row
    let mut dp = pyramid[pyramid.len() - 1].clone();

    // Work upwards, updating the DP array
    for row in (0..pyramid.len() - 1).rev() {
        for col in 0..pyramid[row].len() {
            dp[col] = pyramid[row][col] + dp[col].max(dp[col + 1]);
        }
    }

    // The top of the DP array now contains the maximum path sum
    dp[0]
}

fn main() {
    let pyramid = vec![vec![3], vec![7, 4], vec![2, 4, 6], vec![8, 5, 9, 3]];
    println!("{}", longest_slide_down(&pyramid)); // 23
}

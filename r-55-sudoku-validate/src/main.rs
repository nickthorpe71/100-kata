use std::collections::HashSet;

struct Sudoku {
    data: Vec<Vec<u32>>,
}

impl Sudoku {
    fn is_valid(&self) -> bool {
        let n = self.data.len();
        let sqrt_n = (n as f64).sqrt() as usize;

        // Check if N is a perfect square
        if sqrt_n * sqrt_n != n {
            return false;
        }

        // Helper function to check if a vector contains all numbers from 1 to N exactly once
        let is_valid_vec = |vec: &Vec<u32>| -> bool {
            let mut seen = HashSet::new();
            for &num in vec {
                if num < 1 || num > (n as u32) || !seen.insert(num) {
                    return false;
                }
            }
            true
        };

        // Check rows
        for row in &self.data {
            if row.len() != n || !is_valid_vec(row) {
                return false;
            }
        }

        // Check columns
        for col in 0..n {
            let mut column = Vec::new();
            for row in 0..n {
                column.push(self.data[row][col]);
            }
            if !is_valid_vec(&column) {
                return false;
            }
        }

        // Check all subgrids
        for grid_row in (0..n).step_by(sqrt_n) {
            for grid_col in (0..n).step_by(sqrt_n) {
                let mut subgrid = Vec::new();
                for row in grid_row..grid_row + sqrt_n {
                    for col in grid_col..grid_col + sqrt_n {
                        subgrid.push(self.data[row][col]);
                    }
                }
                if !is_valid_vec(&subgrid) {
                    return false;
                }
            }
        }

        true
    }
}

fn main() {
    let sudoku = Sudoku {
        data: vec![
            vec![7, 8, 4, 1, 5, 9, 3, 2, 6],
            vec![5, 3, 9, 6, 7, 2, 8, 4, 1],
            vec![6, 1, 2, 4, 3, 8, 7, 5, 9],
            vec![9, 2, 8, 7, 1, 5, 4, 6, 3],
            vec![3, 5, 7, 8, 4, 6, 1, 9, 2],
            vec![4, 6, 1, 9, 2, 3, 5, 8, 7],
            vec![8, 7, 6, 3, 9, 4, 2, 1, 5],
            vec![2, 4, 3, 5, 6, 1, 9, 7, 8],
            vec![1, 9, 5, 2, 8, 7, 6, 3, 4]
        ],
    };

    println!("Is the Sudoku valid? {}", sudoku.is_valid());
}

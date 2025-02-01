enum Direction {
    North,
    South,
    East,
    West,
}

fn snail(matrix: &[Vec<i32>]) -> Vec<i32> {
    if matrix.is_empty() || matrix[0].is_empty() {
        return Vec::new();
    }

    let len = matrix.len();
    let mut top = 0;
    let mut bottom = len - 1;
    let mut left = 0;
    let mut right = len - 1;

    let mut res = Vec::new();
    let mut dir = Direction::East;

    while top <= bottom && left <= right {
        match dir {
            Direction::East => {
                for c in left..=right {
                    res.push(matrix[top][c]);
                }
                top += 1;
                dir = Direction::South;
            }
            Direction::South => {
                for r in top..=bottom {
                    res.push(matrix[r][right]);
                }
                right -= 1;
                dir = Direction::West;
            }
            Direction::West => {
                for c in (left..=right).rev() {
                    res.push(matrix[bottom][c]);
                }
                bottom -= 1;
                dir = Direction::North;
            }
            Direction::North => {
                for r in (top..=bottom).rev() {
                    res.push(matrix[r][left]);
                }
                left += 1;
                dir = Direction::East;
            }
        }
    }

    res
}

fn one_layer(n: usize, layer: usize) -> impl Iterator<Item = (usize, usize)> {
    let left_right = (layer..n - layer).map(move |i| (layer, i));
    let up_down = (layer + 1..n - layer).map(move |i| (i, n - 1 - layer));
    let right_left = (layer..n - 1 - layer).rev().map(move |i| (n - 1 - layer, i));
    let down_up = (layer + 1..n - 1 - layer).rev().map(move |i| (i, layer));

    left_right.chain(up_down).chain(right_left).chain(down_up)
}

pub fn snail_best(matrix: &[Vec<i32>]) -> Vec<i32> {
    let n = matrix
        .get(0)
        .map(|xs| xs.len())
        .unwrap_or(0);
    (0..n)
        .flat_map(|x| one_layer(n, x))
        .map(|(x, y)| matrix[x][y])
        .collect()
}

fn main() {
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    let result = snail(&matrix);
    println!("{:?}", result);
}

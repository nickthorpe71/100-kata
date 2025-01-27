use std::collections::HashSet;

fn sum_of_pairs(ints: &[i8], s: i8) -> Option<(i8, i8)> {
    let mut seen = HashSet::new();
    for i in ints {
        let diff = s - i;
        if seen.contains(&diff) {
            return Some((diff, *i));
        }
        seen.insert(i);
    }
    None
}

fn sum_pairs_bit_arr(ints: &[i8], s: i8) -> Option<(i8, i8)> {
    let mut seen: [u8; 32] = [0; 32];

    for y in ints {
        let y = *y;
        let x = s - y;

        // Cast the x/y values into u8 to be used as an index into the array.
        let key_x = x as u8;
        let key_y = y as u8;

        // Check the presence of the requested (x) value.
        if (seen[(key_x >> 3) as usize] & (1 << (key_x & 0x7))) != 0 {
            return Some((x, y));
        }

        // Set the bit for the seen (y) value.
        seen[(key_y >> 3) as usize] |= 1 << (key_y & 0x7);
    }

    None
}

fn main() {
    let l = [1, 4, 8, 7, 3, 15];
    let r = sum_of_pairs(&l, 8);
    println!("{:?}", r); // Some((1, 7))
    let l = [1, -2, 3, 0, -6, 1];
    let r = sum_of_pairs(&l, -6);
    println!("{:?}", r); // Some((0, -6))
    let l = [20, -13, 40];
    let r = sum_of_pairs(&l, -7);
    println!("{:?}", r); // None
    let l = [1, 2, 3, 4, 1, 0];
    let r = sum_of_pairs(&l, 2);
    println!("{:?}", r); // Some((1, 1))
    let l = [10, 5, 2, 3, 7, 5];
    let r = sum_of_pairs(&l, 10);
    println!("{:?}", r); // Some((3, 7))
    let l = [4, -2, 3, 3, 4];
    let r = sum_of_pairs(&l, 8);
    println!("{:?}", r); // Some((4, 4))
    let l = [0, 2, 0];
    let r = sum_of_pairs(&l, 0);
    println!("{:?}", r); // Some((0, 0))
    let l = [5, 9, 13, -3];
    let r = sum_of_pairs(&l, 10);
    println!("{:?}", r); // Some((13, -3))
}

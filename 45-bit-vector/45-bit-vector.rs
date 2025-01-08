fn sort_by_bit(list: &[u8]) -> u32 {
    let mut result = 0;

    for &index in list {
        result |= 1 << index;
    }

    result
}

fn main() {
    let test1 = vec![0, 1];
    let test2 = vec![1, 2, 0, 4];
    
    println!("{}", sort_by_bit(&test1)); // Output: 3
    println!("{}", sort_by_bit(&test2)); // Output: 23
}

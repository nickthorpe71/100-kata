fn count_bits(n: i64) -> u32 {
    n.count_ones()
}

fn main() {
    println!("{}", count_bits(0)); // 0
    println!("{}", count_bits(31)); // 5
    println!("{}", count_bits(89)); // 4
}
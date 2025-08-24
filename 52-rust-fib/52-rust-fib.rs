fn fib(n: i64) -> i64 {
    if n == 0 || n == 1 {
        return n;
    }

    let mut a = 0;
    let mut b = 1;
    let mut c = 0;

    for _ in 2..=n {
        c = a + b;
        a = b;
        b = c;
    }

    c 
}

fn main() {
    println!("{}", fib(0)); // 0
    println!("{}", fib(1)); // 1
    println!("{}", fib(2)); // 1
    println!("{}", fib(3)); // 2
    println!("{}", fib(4)); // 3
    println!("{}", fib(5)); // 5
    println!("{}", fib(6)); // 8
    println!("{}", fib(7)); // 13
    println!("{}", fib(8)); // 21
    println!("{}", fib(9)); // 34
    println!("{}", fib(10)); // 55
    println!("{}", fib(11)); // 89
    println!("{}", fib(12)); // 144
}
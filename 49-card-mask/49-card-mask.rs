fn maskify(cc: &str) -> String {
    if cc.len() < 5 {
        return cc.to_string();
    }
    let mut masked = String::new();
    for (i, c) in cc.chars().enumerate() {
        if i < cc.len() - 4 {
            masked.push('#');
        } else {
            masked.push(c);
        }
    }
    masked
}

fn main() {
    println!("{}", maskify(""));
    println!("{}", maskify("6"));
    println!("{}", maskify("4556364607935616"));
}

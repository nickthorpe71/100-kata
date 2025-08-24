use itertools::Itertools;

fn solution_iter(s: &str) -> Vec<String> {
    s.chars()
        .chunks(2)
        .into_iter()
        .map(|c| c.pad_using(2, |_| '_').collect())
        .collect();
}

fn solution(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let chars: Vec<char> = s.chars().collect();

    let mut i = 0;
    while i < chars.len() {
        let mut pair = String::new();
        pair.push(chars[i]);

        if i + 1 < chars.len() {
            pair.push(chars[i + 1]);
        } else {
            pair.push('_');
        }

        result.push(pair);
        i += 2;
    }

    result
}

fn main() {
    let input = "abcdefg";
    let output = solution(input);
    println!("Input: {}", input);
    println!("Output: {:?}", output);
}

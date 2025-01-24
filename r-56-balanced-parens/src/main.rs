fn balanced_parens(n: u16) -> Vec<String> {
    fn generate(open: u16, close: u16, n: u16) -> Vec<String> {
        if open == n && close == n {
            return vec![String::new()]; // return an empty string to be built upon
        }

        let mut result = Vec::new();

        if open < n {
            result.extend(
                generate(open + 1, close, n)
                    .into_iter()
                    .map(|s| format!("({}", s))
            );
        }

        if close < open {
            result.extend(
                generate(open, close + 1, n)
                    .into_iter()
                    .map(|s| format!("){}", s))
            );
        }

        result
    }

    generate(0, 0, n)
}

fn test(received: Vec<String>, expected: Vec<&str>) {
    let received: Vec<String> = received.into_iter().collect();
    let expected: Vec<String> = expected
        .into_iter()
        .map(|s| s.to_string())
        .collect();
    let passed: bool = received == expected;
    if passed {
        println!("PASSED");
    }
    println!("FAILED | Expected: {:?} | Received: {:?}", expected, received);
}

fn main() {
    test(balanced_parens(0), vec![""]);
    test(balanced_parens(1), vec!["()"]);
    test(balanced_parens(2), vec!["()()", "(())"]);
    test(balanced_parens(3), vec!["()()()", "(())()", "()(())", "(()())", "((()))"]);
}

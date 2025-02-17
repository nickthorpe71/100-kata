use std::env;

fn evaluate_expression(equation_str: &str) -> Result<i32, String> {
    let equation_str: String = equation_str
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();

    let mut total = 0;
    let mut curr_number = String::new();
    let mut last_operator = '+';
    let mut first_char = true;

    for e_char in equation_str.chars() {
        if e_char.is_ascii_digit() {
            curr_number.push(e_char);
        } else if e_char == '+' || e_char == '-' {
            if first_char && e_char == '-' {
                curr_number.push(e_char); // Support for "-5+3"
            } else {
                if curr_number.is_empty() {
                    return Err(format!("Syntax error: unexpected operator '{}'", e_char));
                }
                let num: i32 = curr_number
                    .parse()
                    .map_err(|_| format!("Invalid number: {}", curr_number))?;
                match last_operator {
                    '+' => {
                        total += num;
                    }
                    '-' => {
                        total -= num;
                    }
                    _ => unreachable!(),
                }
                curr_number.clear();
                last_operator = e_char;
            }
        } else {
            return Err(format!("Invalid character encountered: '{}'", e_char));
        }
        first_char = false;
    }

    if !curr_number.is_empty() {
        let num: i32 = curr_number.parse().map_err(|_| format!("Invalid number: {}", curr_number))?;
        match last_operator {
            '+' => {
                total += num;
            }
            '-' => {
                total -= num;
            }
            _ => unreachable!(),
        }
    }

    Ok(total)
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let input_expression = args.concat();

    match evaluate_expression(&input_expression) {
        Ok(result) => println!("Total: {}", result),
        Err(err) => eprintln!("Error: {}", err),
    }
}

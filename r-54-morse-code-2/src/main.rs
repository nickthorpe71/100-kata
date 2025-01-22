use std::collections::HashMap;

fn decode_bits(encoded: &str) -> String {
    let trimmed = encoded.trim_matches('0');

    // Count the shortest sequence of 1s and 0s
    let mut counts = Vec::new();
    let mut prev = trimmed.chars().next().unwrap();
    let mut count = 1;

    for bit in trimmed.chars() {
        if bit == prev {
            count += 1;
        } else {
            counts.push(count);
            count = 1;
            prev = bit;
        }
    }
    counts.push(count);

    let time_unit = *counts.iter().min().unwrap();

    // Convert the bit stream to Morse code
    let mut morse = String::new();
    let mut current_char = trimmed.chars().next().unwrap();

    for bit in trimmed.chars() {
        if bit == current_char {
            count += 1;
        } else {
            if current_char == '1' {
                morse.push(if count / time_unit == 3 { '-' } else { '.' });
            } else if current_char == '0' {
                match count / time_unit {
                    3 => morse.push(' '), // Space between characters
                    7 => morse.push_str("   "), // Space between words
                    _ => {}
                }
            }
            count = 1;
            current_char = bit;
        }
    }

    // Add the last segment
    if current_char == '1' {
        morse.push(if count / time_unit == 3 { '-' } else { '.' });
    }

    morse
}

fn decode_morse(encoded: &str, MORSE_CODE: HashMap<&str, &str>) -> String {
    encoded
    .split(' ')
    // .map(|x| { MORSE_CODE.get(x).unwrap_or(&" ".to_string()).clone() })
    .map(|x| { MORSE_CODE.get(x).unwrap_or(&" ").clone() })
    .collect::<String>()
    .split_whitespace()
    .collect::<Vec<_>>()
    .join(" ")
}

fn main() {

    let morse_code: HashMap<&str, &str> = [
        (".-", "A"), ("-...", "B"), ("-.-.", "C"), ("-..", "D"), (".", "E"),
        ("..-.", "F"), ("--.", "G"), ("....", "H"), ("..", "I"), (".---", "J"),
        ("-.-", "K"), (".-..", "L"), ("--", "M"), ("-.", "N"), ("---", "O"),
        (".--.", "P"), ("--.-", "Q"), (".-.", "R"), ("...", "S"), ("-", "T"),
        ("..-", "U"), ("...-", "V"), (".--", "W"), ("-..-", "X"), ("-.--", "Y"),
        ("--..", "Z"), ("-----", "0"), (".----", "1"), ("..---", "2"),
        ("...--", "3"), ("....-", "4"), (".....", "5"), ("-....", "6"),
        ("--...", "7"), ("---..", "8"), ("----.", "9"),
        ("...---...", "SOS")
    ].iter().cloned().collect();

    let bits= "1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011";
    let morse = decode_bits(bits);
    let decoded = decode_morse(&morse, morse_code);
    println!("{}", decoded); // Output: "HEY JUDE"
}

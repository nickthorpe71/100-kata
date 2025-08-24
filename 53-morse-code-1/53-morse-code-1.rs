use std::collections::HashMap;

fn decode_morse(morse_code: &str) -> String {
    // Preload the Morse code map
    let morse_code_map: HashMap<&str, &str> = [
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

    // Strip leading and trailing whitespace
    let morse_code = morse_code.trim();

    // Split into words (separated by 3 spaces)
    let words = morse_code.split("   ");

    // Decode each word
    let decoded_words: Vec<String> = words
        .map(|word| {
            // Split the word into characters (separated by a single space)
            word.split(' ')
                // Decode each character using the Morse code map
                .filter_map(|char| morse_code_map.get(char).cloned())
                .collect::<String>() // Join characters to form the word
        })
        .collect();

    // Join all words with a space to form the final decoded message
    decoded_words.join(" ")
}

fn decode_morse_clean(encoded: &str) -> String {
    encoded
    .split(' ')
    .map(|x| { MORSE_CODE.get(x).unwrap_or(&" ".to_string()).clone() })
    .collect::<String>()
    .split_whitespace()
    .collect::<Vec<_>>()
    .join(" ")
}

fn main() {
    let morse = ".... . -.--   .--- ..- -.. .";
    let decoded = decode_morse(morse);
    println!("{}", decoded); // Output: "HEY JUDE"
}
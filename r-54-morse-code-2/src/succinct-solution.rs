mod preloaded;
use preloaded::MORSE_CODE;
// MORSE_CODE is `HashMap<String, String>`. e.g. ".-" -> "A".

use itertools::Itertools;
use num::Integer;

pub fn decode_bits(encoded: &str) -> String {
    lazy_static::lazy_static! {
        static ref GROUP: regex::Regex = regex::Regex::new("0+|1+").unwrap();
    };
    let encoded = encoded.trim_matches('0');
    let step = GROUP
        .find_iter(&encoded)
        .map(|_match| _match.as_str().len())
        .fold(0, |div, next| div.gcd(&next));
    return encoded
        .chars()
        .step_by(step)
        .collect::<String>()
        .replace("111", "-")
        .replace("1", ".")
        .replace("0000000", " ")
        .replace("000", "/")
        .replace("0", "");
}

pub fn decode_morse(encoded: &str) -> String {
    return encoded
        .split(' ')
        .map(|w| w.split('/').filter_map(|m| MORSE_CODE.get(m)).join(""))
        .join(" ");
}
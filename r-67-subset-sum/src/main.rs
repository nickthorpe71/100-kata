fn find_average(slice: &[f64]) -> f64 {
    let length: f64 = slice.len() as f64;
    if length < 1.0 {
        return 0.0;
    }
    let sum: f64 = slice.iter().copied().sum();
    sum / length
}

fn avg(slice: &[f64]) -> f64 {
    match slice.len() {
        0 => 0.0,
        n => slice.iter().sum::<f64>() / n as f64
    }
}

fn main() {
    let input = [
        17.0, 16.0, 16.0, 16.0, 16.0, 15.0, 17.0, 17.0, 15.0, 5.0, 17.0, 17.0, 16.0,
    ];
    let average = avg(&input);
    println!("{}", average);
}

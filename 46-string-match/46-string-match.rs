fn match_counts_imperative(a1: &[String], a2: &[String]) -> Vec<usize> {
    let mut result = Vec::new();

    for item in a2 {
        let mut count = 0;
        for elem in a1 {
            if item == elem {
                count += 1;
            }
        }
        result.push(count)
    }

    result
}

fn match_counts(a1: &[String], a2: &[String]) -> Vec<usize> {
    a2.iter()
        .map(|item| a1.iter().filter(|&x| x == item).count())
        .collect()
}

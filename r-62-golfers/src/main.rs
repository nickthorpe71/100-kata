use std::collections::HashMap;
use std::collections::HashSet;

fn valid(a: Vec<Vec<&str>>) -> bool {
    let first_group_size = a[0][0].len();
    let mut matchups: HashMap<char, HashSet<char>> = HashMap::new();
    let mut all_players: HashSet<char> = HashSet::new();

    for day in &a {
        let mut players_in_day: HashSet<char> = HashSet::new();

        for group in day {
            // 1. Check group size consistency
            if group.len() != first_group_size {
                return false;
            }

            let players: Vec<char> = group.chars().collect();
            let mut new_matchups: Vec<(char, char)> = Vec::new();

            for i in 0..players.len() {
                let player_i = players[i];

                // Add player to the overall player set and the current day's set
                all_players.insert(player_i);
                if !players_in_day.insert(player_i) {
                    return false; // Player plays more than once in a day
                }

                // 2. Check and prepare matchups without modifying the map yet
                if let Some(player_i_matchups) = matchups.get(&player_i) {
                    for j in 0..players.len() {
                        if i == j {
                            continue; // Skip self-match
                        }

                        let player_j = players[j];

                        if player_i_matchups.contains(&player_j) {
                            return false; // Duplicate matchup found
                        }

                        // Queue the new matchup for insertion
                        new_matchups.push((player_i, player_j));
                    }
                } else {
                    // If player_i has no matchups yet, queue all new matchups
                    for j in 0..players.len() {
                        if i != j {
                            new_matchups.push((player_i, players[j]));
                        }
                    }
                }
            }

            // 3. Apply the new matchups after checks
            for (p1, p2) in new_matchups {
                matchups.entry(p1).or_insert(HashSet::new()).insert(p2);
                matchups.entry(p2).or_insert(HashSet::new()).insert(p1);
            }
        }

        // 4. Ensure all players are present for the day
        if players_in_day.len() != all_players.len() {
            return false; // Missing or extra players
        }
    }

    true
}

fn valid_old(a: Vec<Vec<&str>>) -> bool {
    // check group size
    let first_group_size = a[0][0].len();
    for day in &a {
        for group in day {
            let curr_group_size = group.len();
            if first_group_size != curr_group_size {
                return false;
            }
        }
    }

    // check that each player plays each other player a max of once
    let mut matchups: HashMap<char, Vec<char>> = HashMap::new();

    for day in &a {
        for group in day {
            let players: Vec<char> = group.chars().collect();
            let mut new_matchups: Vec<(char, char)> = Vec::new();
            for i in 0..players.len() {
                if let Some(player_i_matchups) = matchups.get(&players[i]) {
                    for j in 0..players.len() {
                        if j == i {
                            continue; // Don't match with self
                        }

                        // Check if the matchup already exists
                        if player_i_matchups.contains(&players[j]) {
                            return false; // Duplicate matchup found
                        }

                        // Queue the new matchup for insertion
                        new_matchups.push((players[i], players[j]));
                    }
                } else {
                    // If player_i has no matchups yet, we can queue all players
                    for j in 0..players.len() {
                        if j != i {
                            new_matchups.push((players[i], players[j]));
                        }
                    }
                }
            }
            for (p1, p2) in new_matchups {
                matchups.entry(p1).or_insert(Vec::new()).push(p2);
                matchups.entry(p2).or_insert(Vec::new()).push(p1);
            }
        }
    }

    // check each player plays once a day no more no less
    for day in &a {
        let mut players_in_day: HashSet<char> = HashSet::new();
        for group in day {
            for player in group.chars() {
                if players_in_day.contains(&player) {
                    return false; // Player plays more than once in a day
                }
                players_in_day.insert(player);
            }
        }
    }

    true
}

fn main() {
    let a = vec![
        vec!["ABCD", "EFGH", "IJKL", "MNOP", "QRST"],
        vec!["AEIM", "BJOQ", "CHNT", "DGLS", "FKPR"],
        vec!["AGKO", "BIPT", "CFMS", "DHJR", "ELNQ"],
        vec!["AHLP", "BKNS", "CEOR", "DFIQ", "GJMT"],
        vec!["AFJN", "BLMR", "CGPQ", "DEKT", "HIOS"]
    ];

    let b = vec![
        vec!["ABCD", "EFGH", "IJKL", "MNOP", "QRST"],
        vec!["ABIM", "BEOQ", "CHNT", "DGLS", "FKPR"],
        vec!["AGKO", "BIPT", "CFMS", "DHJR", "ELNQ"],
        vec!["AHLP", "BKNS", "CEOR", "DFIQ", "GJMT"],
        vec!["AFJN", "BLMR", "CGPQ", "DEKT", "HIOS"]
    ];

    let c = vec![
        vec!["BCD", "EFGH", "IJKL", "MNOP", "QRST"],
        vec!["AEIM", "BJOQ", "CHNT", "DGLS", "FKPR"],
        vec!["AGKO", "BIPT", "CFMS", "DHJR", "ELNQ"],
        vec!["AHLP", "BKNS", "CEOR", "DFIQ", "GJMT"],
        vec!["AFJN", "BLMR", "CGPQ", "DEKT", "HIOS"]
    ];

    let d = vec![
        vec!["QBCD", "EFGH", "IJKL", "MNOP", "QRST"],
        vec!["AEIM", "BJOQ", "CHNT", "DGLS", "FKPR"],
        vec!["AGKO", "BIPT", "CFMS", "DHJR", "ELNQ"],
        vec!["AHLP", "BKNS", "CEOR", "DFIQ", "GJMT"],
        vec!["AFJN", "BLMR", "CGPQ", "DEKT", "HIOS"]
    ];

    let e = vec![vec!["AB"]];

    let f = vec![vec!["AB", "CD"], vec!["AD", "BC"], vec!["BD", "AC"]];

    println!("{}", valid(a)); // true
    println!("{}", valid(b)); // false A paired with B twice
    println!("{}", valid(c)); // false not all same group size
    println!("{}", valid(d)); // false A doesn't play on the first day
    println!("{}", valid(e)); // true
    println!("{}", valid(f)); // true
}

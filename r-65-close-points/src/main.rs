use core::f64;
use std::cmp::Ordering;

fn distance_squared(p1: (f64, f64), p2: (f64, f64)) -> f64 {
    let dx = p1.0 - p2.0;
    let dy = p1.1 - p2.1;
    dx * dx + dy * dy
}

fn brute_force(points: &[(f64, f64)]) -> ((f64, f64), (f64, f64), f64) {
    let mut min_dist = f64::MAX;
    let mut closest_pair = ((0.0, 0.0), (0.0, 0.0));

    for i in 0..points.len() {
        for j in i + 1..points.len() {
            let dist = distance_squared(points[i], points[j]);
            if dist < min_dist {
                min_dist = dist;
                closest_pair = (points[i], points[j]);
            }
        }
    }
    (closest_pair.0, closest_pair.1, min_dist)
}

fn closest_in_strip(strip: &[(f64, f64)], d: f64) -> ((f64, f64), (f64, f64), f64) {
    let mut min_dist = d;
    let mut closest_pair = ((0.0, 0.0), (0.0, 0.0));

    let mut sorted_strip = strip.to_vec();
    sorted_strip.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(Ordering::Equal));

    for i in 0..sorted_strip.len() {
        for j in i + 1..sorted_strip.len() {
            if
                (sorted_strip[j].1 - sorted_strip[i].1) * (sorted_strip[j].1 - sorted_strip[i].1) >=
                min_dist
            {
                break;
            }
            let dist = distance_squared(sorted_strip[i], sorted_strip[j]);
            if dist < min_dist {
                min_dist = dist;
                closest_pair = (sorted_strip[i], sorted_strip[j]);
            }
        }
    }
    (closest_pair.0, closest_pair.1, min_dist)
}

fn closest_recursive(points: &[(f64, f64)]) -> ((f64, f64), (f64, f64), f64) {
    if points.len() <= 3 {
        return brute_force(points);
    }

    let mid = points.len() / 2;
    let left = &points[..mid];
    let right = &points[mid..];

    let (p1, q1, d1) = closest_recursive(left);
    let (p2, q2, d2) = closest_recursive(right);

    let (mut best_pair, mut best_dist) = if d1 < d2 { ((p1, q1), d1) } else { ((p2, q2), d2) };

    let mid_x = points[mid].0;
    let strip: Vec<(f64, f64)> = points
        .iter()
        .cloned()
        .filter(|p| {
            let dx = p.0 - mid_x;
            dx * dx < best_dist
        })
        .collect();

    let (sp1, sp2, sp_dist) = closest_in_strip(&strip, best_dist);
    if sp_dist < best_dist {
        best_pair = (sp1, sp2);
        best_dist = sp_dist;
    }

    (best_pair.0, best_pair.1, best_dist)
}

pub fn closest_pair(points: &[(f64, f64)]) -> ((f64, f64), (f64, f64)) {
    let mut sorted_points = points.to_vec();
    sorted_points.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(Ordering::Equal));

    let (p1, p2, _) = closest_recursive(&sorted_points);
    (p1, p2)
}

fn closest_pair_iter(points: &[(f64, f64)]) -> ((f64, f64), (f64, f64)) {
    let n = points.len();
    let mut l = f64::INFINITY;
    let mut a = 0;
    let mut b = 0;
    let mut tolerance = l.sqrt();
    let mut arr: Vec<(f64, f64)> = points.iter().cloned().collect();
    arr.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    for i in 0..n - 1 {
        for j in i + 1..n {
            if arr[j].0 >= arr[i].0 + tolerance {
                break;
            } else {
                let ls =
                    (arr[i].0 - arr[j].0) * (arr[i].0 - arr[j].0) +
                    (arr[i].1 - arr[j].1) * (arr[i].1 - arr[j].1);

                if ls < l {
                    l = ls;
                    tolerance = l.sqrt();
                    a = i;
                    b = j;
                }
            }
        }
    }

    (arr[a], arr[b])
}

fn main() {
    let points = vec![
        (2.0, 2.0),
        (2.0, 8.0),
        (5.0, 5.0),
        (6.0, 3.0),
        (6.0, 7.0),
        (7.0, 4.0),
        (7.0, 9.0)
    ];
    let start = std::time::Instant::now();
    let (p1, p2) = closest_pair(&points);
    println!("Time: {:?}", start.elapsed());
    println!("Closest pair: {:?}, {:?}", p1, p2);

    let start = std::time::Instant::now();
    let (p1, p2) = closest_pair_iter(&points);
    println!("Time: {:?}", start.elapsed());
    println!("Closest pair: {:?}, {:?}", p1, p2);
}

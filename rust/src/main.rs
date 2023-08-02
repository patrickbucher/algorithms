use std::collections::HashMap;

fn factorial(n: i64) -> i64 {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() {
    let milliseconds_multipliers: Vec<i64> = vec![1e6 as i64, 60, 60, 24, 30, 12, 100];
    let periods: Vec<&str> = vec![
        "second", "minute", "hour", "day", "month", "year", "century",
    ];

    let orders: Vec<&str> = vec!["lg n", "√n", "n", "n lg n", "n²", "n³", "2^n", "n!"].to_vec();
    let order_funcs: Vec<fn(f64) -> f64> = vec![
        |n: f64| -> f64 { n.log10() },
        |n: f64| -> f64 { n.sqrt() },
        |n: f64| -> f64 { n },
        |n: f64| -> f64 { n * n.log10() },
        |n: f64| -> f64 { n.powi(2) },
        |n: f64| -> f64 { n.powi(3) },
        |n: f64| -> f64 { 2_f64.powf(n) },
        |n: f64| -> f64 { factorial(n as i64) as f64 },
    ]
    .to_vec();

    let mut timings: HashMap<&str, i64> = HashMap::new();
    for i in 0..milliseconds_multipliers.len() {
        let mut multiplier: i64 = 1;
        for j in 0..(i + 1) {
            multiplier *= milliseconds_multipliers[j];
        }
        timings.insert(periods[i], multiplier);
    }
    for o in 0..orders.len() {
        for p in 0..periods.len() {
            let ms = timings.get(periods[p]).unwrap();
            let f = order_funcs[o];
            let mut i: f64 = 0.0;
            let mut n: f64 = 10.0;
            loop {
                if f(n) < (*ms as f64) {
                    i = n;
                    n *= 1.001;
                } else {
                    println!("{} {}: n={:.3e}", orders[o], periods[p], i);
                    break;
                }
            }
        }
    }
}

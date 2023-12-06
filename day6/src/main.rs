fn main() {
    #[rustfmt::skip]
    println!("{:?}", solve("Time:        48     87     69     81
                            Distance:   255   1288   1117   1623"));

    #[rustfmt::skip]
    println!("{:?}", solve("Time:       48876981
                            Distance:   25528811171623"));
}

fn solve(input: &str) -> Option<i64> {
    let mut lines = input.lines();

    let times = lines
        .next()?
        .split(&" ")
        .filter_map(|s| s.parse::<i64>().ok())
        .collect::<Vec<i64>>();

    let distances = lines
        .next()?
        .split(&" ")
        .filter_map(|s| s.parse::<i64>().ok())
        .collect::<Vec<i64>>();

    let solution = times
        .iter()
        .zip(distances.iter())
        .map(|(t, s_min)| ways_to_win(*t, *s_min))
        .product();

    Some(solution)
}

#[inline]
fn ways_to_win(t_limit: i64, s_min: i64) -> i64 {
    let mut t_min = -1;
    let mut t_max = -1;

    for t in 1..t_limit {
        let s = dist(t, t_limit);

        if t_min < 0 {
            if s > s_min {
                t_min = t;
            }
        } else if t_max < 0 {
            if s <= s_min {
                t_max = t;
                break;
            }
        }
    }

    t_max - t_min
}

#[inline]
fn dist(hold_time: i64, time_limit: i64) -> i64 {
    let t = time_limit - hold_time;
    let v = hold_time;
    v * t
}

use std::{collections::VecDeque, fmt::Display, time::Instant};

#[allow(unused_imports)]
use proconio::*;
#[allow(unused_imports)]
use rand::prelude::*;
use rand_pcg::Pcg64Mcg;

#[allow(unused_macros)]
macro_rules! chmin {
    ($base:expr, $($cmps:expr),+ $(,)*) => {{
        let cmp_min = min!($($cmps),+);
        if $base > cmp_min {
            $base = cmp_min;
            true
        } else {
            false
        }
    }};
}

#[allow(unused_macros)]
macro_rules! chmax {
    ($base:expr, $($cmps:expr),+ $(,)*) => {{
        let cmp_max = max!($($cmps),+);
        if $base < cmp_max {
            $base = cmp_max;
            true
        } else {
            false
        }
    }};
}

#[allow(unused_macros)]
macro_rules! min {
    ($a:expr $(,)*) => {{
        $a
    }};
    ($a:expr, $b:expr $(,)*) => {{
        std::cmp::min($a, $b)
    }};
    ($a:expr, $($rest:expr),+ $(,)*) => {{
        std::cmp::min($a, min!($($rest),+))
    }};
}

#[allow(unused_macros)]
macro_rules! max {
    ($a:expr $(,)*) => {{
        $a
    }};
    ($a:expr, $b:expr $(,)*) => {{
        std::cmp::max($a, $b)
    }};
    ($a:expr, $($rest:expr),+ $(,)*) => {{
        std::cmp::max($a, max!($($rest),+))
    }};
}

#[allow(unused_macros)]
macro_rules! mat {
    ($e:expr; $d:expr) => { vec![$e; $d] };
    ($e:expr; $d:expr $(; $ds:expr)+) => { vec![mat![$e $(; $ds)*]; $d] };
}

#[derive(Debug, Clone)]
struct Input {
    map_size: usize,
    district_count: usize,
    merged_count: usize,
    average_population: i64,
    average_staff: i64,
    districts: Vec<District>,
    map: Vec<Vec<usize>>,
    since: Instant,
}

impl Input {
    fn read_input() -> Self {
        input! {
            map_size: usize,
            district_count: usize,
            merged_count: usize,
        }

        let since = Instant::now();
        let mut districts = vec![];
        let mut total_population = 0;
        let mut total_staff = 0;

        for _ in 0..district_count {
            input! {
                population: i64,
                staff: i64
            }

            total_population += population;
            total_staff += staff;
            districts.push(District::new(population, staff));
        }

        let average_population = total_population / district_count as i64;
        let average_staff = total_staff / district_count as i64;

        let mut raw_map = vec![];

        for _ in 0..map_size {
            input! {
                row: [usize; map_size]
            }

            raw_map.push(row);
        }

        let map = Self::gen_map(raw_map, map_size, district_count);

        Self {
            map_size,
            district_count,
            merged_count,
            average_population,
            average_staff,
            districts,
            map,
            since,
        }
    }

    fn gen_map(
        raw_map: Vec<Vec<usize>>,
        map_size: usize,
        district_count: usize,
    ) -> Vec<Vec<usize>> {
        let mut map = vec![vec![]; district_count];

        for row in 0..map_size {
            for col in 0..map_size {
                let district = raw_map[row][col].wrapping_sub(1);

                if district >= district_count {
                    continue;
                }

                for &(dr, dc) in &[(0, 1), (0, !0), (1, 0), (!0, 0)] {
                    let nr = row.wrapping_add(dr);
                    let nc = col.wrapping_add(dc);

                    if nr < map_size && nc < map_size {
                        let next = raw_map[nr][nc].wrapping_sub(1);

                        if next < district_count && next != district {
                            map[district].push(next);
                        }
                    }
                }
            }
        }

        for m in map.iter_mut() {
            m.sort_unstable();
            m.dedup();
        }

        map
    }
}

#[derive(Debug, Clone, Copy)]
struct District {
    population: i64,
    staff: i64,
}

impl District {
    fn new(population: i64, staff: i64) -> Self {
        Self { population, staff }
    }
}

#[derive(Debug, Clone)]
struct State {
    assigns: Vec<usize>,
    assign_counts: Vec<i32>,
}

impl State {
    fn init(input: &Input, seed: u128) -> Self {
        let mut rng = Pcg64Mcg::new(seed);
        let mut assigns = vec![!0; input.district_count];

        for i in 0..input.merged_count {
            loop {
                let j = rng.gen_range(0, input.district_count);

                if assigns[j] == !0 {
                    assigns[j] = i;
                    break;
                }
            }
        }

        let mut queue = VecDeque::new();
        for (i, &assign) in assigns.iter().enumerate() {
            if assign != !0 {
                queue.push_back((i, assign));
            }
        }

        while let Some((i, assign)) = queue.pop_front() {
            for &next in input.map[i].iter() {
                if assigns[next] == !0 {
                    assigns[next] = assign;
                    queue.push_back((next, assign));
                }
            }
        }

        let mut assign_counts = vec![0; input.merged_count];

        for assign in assigns.iter() {
            assign_counts[*assign] += 1;
        }

        Self {
            assigns,
            assign_counts,
        }
    }

    fn calc_score(&self, input: &Input) -> i64 {
        let mut population_counts = vec![0; input.merged_count];
        let mut staff_counts = vec![0; input.merged_count];

        for (assign, district) in self.assigns.iter().zip(input.districts.iter()) {
            population_counts[*assign] += district.population;
            staff_counts[*assign] += district.staff;
        }

        let min_population = *population_counts.iter().min().unwrap() as f64;
        let max_population = *population_counts.iter().max().unwrap() as f64;
        let min_staff = *staff_counts.iter().min().unwrap() as f64;
        let max_staff = *staff_counts.iter().max().unwrap() as f64;

        let min = (min_population / max_population).min(min_staff / max_staff);
        let score = (1e6 * min).round() as i64;
        score
    }
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.assigns[0] + 1)?;

        for assign in self.assigns[1..].iter() {
            writeln!(f)?;
            write!(f, "{}", assign + 1)?;
        }

        Ok(())
    }
}

fn main() {
    let input = Input::read_input();
    let state = solve(&input);
    println!("{}", &state);
    eprintln!("score: {}", state.calc_score(&input));
}

fn solve(input: &Input) -> State {
    let init_state = State::init(input, 42);
    init_state
}

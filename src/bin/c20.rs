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
    seen_dfs: Vec<bool>,
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

        let seen_dfs = vec![false; input.district_count];

        Self {
            assigns,
            assign_counts,
            seen_dfs,
        }
    }

    fn change_assign(&mut self, district: usize, assign: usize) {
        let old = self.assigns[district];
        self.assign_counts[old] -= 1;
        self.assign_counts[assign] += 1;
        self.assigns[district] = assign;
    }

    fn check_connected(
        &mut self,
        input: &Input,
        changed_district: usize,
        target_merged: usize,
    ) -> bool {
        fn dfs(state: &mut State, input: &Input, current: usize, target: usize) -> i32 {
            state.seen_dfs[current] = true;
            let mut count = 1;

            for &next in input.map[current].iter() {
                if state.assigns[next] == target && !state.seen_dfs[next] {
                    count += dfs(state, input, next, target);
                }
            }

            count
        }

        fn clean_dfs(state: &mut State, input: &Input, current: usize) {
            state.seen_dfs[current] = false;

            for &next in input.map[current].iter() {
                if state.seen_dfs[next] {
                    clean_dfs(state, input, next);
                }
            }
        }

        for &next in input.map[changed_district].iter() {
            if self.assigns[next] != target_merged {
                continue;
            }

            let ok = dfs(self, input, next, target_merged) == self.assign_counts[target_merged];
            clean_dfs(self, input, next);
            return ok;
        }

        false
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
    let elapsed = (Instant::now() - input.since).as_secs_f64();
    println!("{}", &state);
    eprintln!("Score = {}", state.calc_score(&input));
    eprintln!("elapsed: {:.3}s", elapsed);
}

fn solve(input: &Input) -> State {
    let init_state = State::init(input, 42);
    let elapsed = (Instant::now() - input.since).as_secs_f64();
    let state = annealing(input, init_state, 0.99 - elapsed);
    state
}

fn annealing(input: &Input, initial_state: State, duration: f64) -> State {
    let mut state = initial_state;
    let mut best_state = state.clone();
    let mut current_score = state.calc_score(input);
    let mut best_score = current_score;

    let mut all_iter = 0;
    let mut valid_iter = 0;
    let mut accepted_count = 0;
    let mut update_count = 0;
    let mut rng = rand_pcg::Pcg64Mcg::new(42);

    let duration_inv = 1.0 / duration;
    let since = std::time::Instant::now();
    let mut time = 0.0;

    let temp0 = 1e4;
    let temp1 = 1e3;
    let mut inv_temp = 1.0 / temp0;

    while time < 1.0 {
        all_iter += 1;
        if (all_iter & ((1 << 10) - 1)) == 0 {
            time = (std::time::Instant::now() - since).as_secs_f64() * duration_inv;
            let temp = f64::powf(temp0, 1.0 - time) * f64::powf(temp1, time);
            inv_temp = 1.0 / temp;
        }

        // 変形
        let pivot = rng.gen_range(0, input.district_count);
        let target_district = *input.map[pivot].choose(&mut rng).unwrap();
        let new_assign = state.assigns[pivot];
        let old_assign = state.assigns[target_district];

        if new_assign == old_assign || state.assign_counts[old_assign] == 1 {
            continue;
        }

        state.change_assign(target_district, new_assign);

        if !state.check_connected(input, target_district, old_assign) {
            state.change_assign(target_district, old_assign);
        }

        // スコア計算
        let new_score = state.calc_score(input);
        let score_diff = new_score - current_score;

        if score_diff >= 0 || rng.gen_bool(f64::exp(score_diff as f64 * inv_temp)) {
            // 解の更新
            current_score = new_score;
            accepted_count += 1;

            if chmax!(best_score, current_score) {
                best_state = state.clone();
                update_count += 1;
            }
        } else {
            state.change_assign(target_district, old_assign);
        }

        valid_iter += 1;
    }

    eprintln!("===== annealing =====");
    eprintln!("score      : {}", best_score);
    eprintln!("all iter   : {}", all_iter);
    eprintln!("valid iter : {}", valid_iter);
    eprintln!("accepted   : {}", accepted_count);
    eprintln!("updated    : {}", update_count);
    eprintln!("");

    best_state
}

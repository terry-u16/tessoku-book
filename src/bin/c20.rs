use std::{fmt::Display, time::Instant};

use light_set::LightSet;
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
    _map_size: usize,
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
            _map_size: map_size,
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
    assigns_inv: Vec<LightSet>,
    assign_counts: Vec<i32>,
    seen_dfs: Vec<bool>,
    populations: Vec<i64>,
    staffs: Vec<i64>,
    annealing_score: i64,
}

impl State {
    fn new(input: &Input, assigns: Vec<usize>) -> Self {
        let mut assign_counts = vec![0; input.merged_count];
        let mut assigns_inv = vec![LightSet::new(input.district_count); input.merged_count];
        let mut populations = vec![0; input.merged_count];
        let mut staffs = vec![0; input.merged_count];

        for (i, &assign) in assigns.iter().enumerate() {
            assign_counts[assign] += 1;
            assigns_inv[assign].add(i);
            populations[assign] += input.districts[i].population;
            staffs[assign] += input.districts[i].staff;
        }

        let mut annealing_score = 0;

        for (&population, &staff) in populations.iter().zip(staffs.iter()) {
            annealing_score += State::calc_annealing_score_one(input, population, staff);
        }

        let seen_dfs = vec![false; input.district_count];

        Self {
            assigns,
            assigns_inv,
            assign_counts,
            seen_dfs,
            populations,
            staffs,
            annealing_score,
        }
    }

    fn change_assign_without_score_calc(&mut self, district: usize, assign: usize) {
        let old = self.assigns[district];
        self.assign_counts[old] -= 1;
        self.assign_counts[assign] += 1;
        self.assigns[district] = assign;
    }

    fn change_assign(&mut self, input: &Input, district: usize, assign: usize) {
        let old = self.assigns[district];
        self.change_assign_without_score_calc(district, assign);

        self.assigns_inv[old].remove(district);
        self.assigns_inv[assign].add(district);

        let p = self.populations[old];
        let s = self.staffs[old];
        self.annealing_score -= State::calc_annealing_score_one(input, p, s);
        let p = self.populations[assign];
        let s = self.staffs[assign];
        self.annealing_score -= State::calc_annealing_score_one(input, p, s);

        let district = &input.districts[district];
        self.populations[old] -= district.population;
        self.staffs[old] -= district.staff;
        self.populations[assign] += district.population;
        self.staffs[assign] += district.staff;

        let p = self.populations[old];
        let s = self.staffs[old];
        self.annealing_score += State::calc_annealing_score_one(input, p, s);
        let p = self.populations[assign];
        let s = self.staffs[assign];
        self.annealing_score += State::calc_annealing_score_one(input, p, s);
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

    fn calc_annealing_score(&self, input: &Input) -> i64 {
        let mut population_counts = vec![0; input.merged_count];
        let mut staff_counts = vec![0; input.merged_count];

        for (assign, district) in self.assigns.iter().zip(input.districts.iter()) {
            population_counts[*assign] += district.population;
            staff_counts[*assign] += district.staff;
        }

        let mut score = 0;

        for population in population_counts.iter() {
            let diff = input.average_population - population;
            score -= diff * diff;
        }

        for staff in staff_counts.iter() {
            let diff = input.average_staff - staff;
            score -= diff * diff * 2500;
        }

        score
    }

    fn calc_annealing_score_one(input: &Input, population: i64, staff: i64) -> i64 {
        let mut score = 0;
        let diff = input.average_population - population;
        score -= diff * diff;
        let diff = input.average_staff - staff;
        score -= diff * diff * 2500;
        score
    }

    fn calc_score(&self) -> i64 {
        let min_population = *self.populations.iter().min().unwrap() as f64;
        let max_population = *self.populations.iter().max().unwrap() as f64;
        let min_staff = *self.staffs.iter().min().unwrap() as f64;
        let max_staff = *self.staffs.iter().max().unwrap() as f64;

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

#[derive(Debug, Clone, Copy)]
struct Parameter {
    temp_high: f64,
    temp_low: f64,
    duration: f64,
}

impl Parameter {
    fn new() -> Self {
        let temp_high = 1e10;
        let temp_low = 3e7;
        let duration_mul =
            std::env::var("DURATION_MUL").map_or_else(|_| 1.0, |val| val.parse::<f64>().unwrap());
        let duration = 0.99 * duration_mul;

        Self {
            temp_high,
            temp_low,
            duration,
        }
    }
}

fn main() {
    let input = Input::read_input();
    let params = Parameter::new();
    let state = solve(&input, &params);
    let elapsed = (Instant::now() - input.since).as_secs_f64();
    println!("{}", &state);
    eprintln!("Score = {}", state.calc_score());
    eprintln!("elapsed: {:.3}s", elapsed);
}

fn solve(input: &Input, params: &Parameter) -> State {
    let init_state = get_init_random(input);
    let elapsed = (Instant::now() - input.since).as_secs_f64();
    eprintln!("elapsed: {:.3}s", elapsed);
    let state = annealing(input, params, init_state, params.duration - elapsed);
    state
}

fn get_init_random(input: &Input) -> State {
    let mut best_state = gen_init(input, 42);
    let mut best_score = best_state.calc_annealing_score(input);

    for seed in 0..50 {
        let state = gen_init(input, seed);
        if chmax!(best_score, state.calc_annealing_score(input)) {
            best_state = state;
        }
    }

    best_state
}

fn gen_init(input: &Input, seed: u128) -> State {
    let mut rng = Pcg64Mcg::new(seed);

    let mut assigns = vec![!0; input.district_count];
    let mut population_count = vec![0; input.merged_count];
    let mut staff_count = vec![0; input.merged_count];

    for i in 0..input.merged_count {
        loop {
            let j = rng.gen_range(0, input.district_count);

            if assigns[j] == !0 {
                assigns[j] = i;
                population_count[i] += input.districts[j].population;
                staff_count[i] += input.districts[j].staff;
                break;
            }
        }
    }

    for _ in 0..(input.district_count - input.merged_count) {
        let mut best_score = std::i64::MIN;
        let mut best_op = (!0, !0);

        for (i, &assign) in assigns.iter().enumerate() {
            if assign == !0 {
                continue;
            }

            for &next in input.map[i].iter() {
                if assigns[next] != !0 {
                    continue;
                }

                let mut population = population_count[assign];
                let mut staff = staff_count[assign];
                let current_score = State::calc_annealing_score_one(input, population, staff);
                population += input.districts[next].population;
                staff += input.districts[next].staff;
                let new_score = State::calc_annealing_score_one(input, population, staff);

                if chmax!(best_score, new_score - current_score) {
                    best_op = (next, assign);
                }
            }
        }

        let (i, assign) = best_op;
        assigns[i] = assign;
        population_count[assign] += input.districts[i].population;
        staff_count[assign] += input.districts[i].staff;
    }

    State::new(input, assigns)
}

fn annealing(input: &Input, params: &Parameter, initial_state: State, duration: f64) -> State {
    let mut state = initial_state;
    let mut best_state = state.clone();
    let mut current_score = state.annealing_score;
    let mut best_score = state.calc_score();

    let mut all_iter = 0;
    let mut valid_iter = 0;
    let mut accepted_count = 0;
    let mut update_count = 0;
    let mut rng = rand_pcg::Pcg64Mcg::new(42);

    let duration_inv = 1.0 / duration;
    let since = std::time::Instant::now();
    let mut time = 0.0;

    let temp0 = params.temp_high;
    let temp1 = params.temp_low;
    let mut inv_temp = 1.0 / temp0;
    let mut swap_candidates = vec![];
    let mut swap_candidates_seen = vec![false; input.district_count];

    loop {
        all_iter += 1;
        if (all_iter & ((1 << 10) - 1)) == 0 {
            time = (std::time::Instant::now() - since).as_secs_f64() * duration_inv;
            let temp = f64::powf(temp0, 1.0 - time) * f64::powf(temp1, time);
            inv_temp = 1.0 / temp;
        }

        if time >= 1.0 {
            break;
        }

        // ??????
        let pivot = rng.gen_range(0, input.district_count);
        let target_district1 = *input.map[pivot].choose(&mut rng).unwrap();
        let new_assign = state.assigns[pivot];
        let old_assign = state.assigns[target_district1];

        if new_assign == old_assign || state.assign_counts[old_assign] == 1 {
            continue;
        }

        let swap_neighbor = rng.gen_bool(time);

        if !swap_neighbor {
            // 1????????????
            state.change_assign(input, target_district1, new_assign);

            // ???????????????
            let new_score = state.annealing_score;
            let score_diff = new_score - current_score;

            // ??????check???????????????????????????????????????check??????????????????
            // tomerun???????????????????????????????????????
            if (score_diff >= 0 || rng.gen_bool(f64::exp(score_diff as f64 * inv_temp)))
                && state.check_connected(input, target_district1, old_assign)
            {
                // ????????????
                current_score = new_score;
                accepted_count += 1;

                if chmax!(best_score, state.calc_score()) {
                    best_state = state.clone();
                    update_count += 1;
                }
            } else {
                state.change_assign(input, target_district1, old_assign);
            }
        } else {
            // 1????????????????????????1????????????
            for &i in state.assigns_inv[new_assign].iter() {
                for &next in input.map[i].iter() {
                    if state.assigns[next] == old_assign
                        && next != target_district1
                        && !swap_candidates_seen[next]
                    {
                        swap_candidates.push(i);
                        swap_candidates_seen[i] = true;
                    }
                }
            }

            if swap_candidates.len() == 0 {
                continue;
            }

            let target_district2 = *swap_candidates.choose(&mut rng).unwrap();

            while let Some(v) = swap_candidates.pop() {
                swap_candidates_seen[v] = false;
            }

            state.change_assign(input, target_district1, new_assign);
            state.change_assign(input, target_district2, old_assign);

            // ???????????????
            let new_score = state.annealing_score;
            let score_diff = new_score - current_score;

            if (score_diff >= 0 || rng.gen_bool(f64::exp(score_diff as f64 * inv_temp)))
                && state.check_connected(input, target_district1, old_assign)
                && state.check_connected(input, target_district2, new_assign)
            {
                // ????????????
                current_score = new_score;
                accepted_count += 1;

                if chmax!(best_score, state.calc_score()) {
                    best_state = state.clone();
                    update_count += 1;
                }
            } else {
                state.change_assign(input, target_district1, old_assign);
                state.change_assign(input, target_district2, new_assign);
            }
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

#[allow(dead_code)]
mod light_set {
    #[derive(Debug, Clone)]
    pub struct LightSet {
        values: Vec<usize>,
        positions: Vec<usize>,
    }

    impl LightSet {
        pub fn new(n: usize) -> Self {
            let values = vec![];
            let positions = vec![!0; n];
            Self { values, positions }
        }

        pub fn add(&mut self, v: usize) {
            debug_assert!(!self.contains(v));
            self.positions[v] = self.values.len();
            self.values.push(v);
        }

        pub fn remove(&mut self, v: usize) {
            debug_assert!(self.contains(v));
            let position = self.positions[v];
            let last = *self.values.last().unwrap();
            self.values[position] = last;
            self.values.pop();
            self.positions[last] = position;
            self.positions[v] = !0;
        }

        pub fn clear(&mut self) {
            while let Some(v) = self.values.pop() {
                self.positions[v] = !0;
            }
        }

        pub fn iter(&self) -> core::slice::Iter<'_, usize> {
            self.values.iter()
        }

        pub fn contains(&self, v: usize) -> bool {
            self.positions[v] != !0
        }
    }

    #[cfg(test)]
    mod test {
        use itertools::Itertools;

        use super::LightSet;

        #[test]
        fn add_test() {
            let mut set = LightSet::new(5);
            set.add(0);
            set.add(2);
            set.add(4);

            let result = set.iter().copied().sorted().collect_vec();
            assert_eq!(result, vec![0, 2, 4]);
        }

        #[test]
        fn remove_test() {
            let mut set = LightSet::new(5);
            set.add(0);
            set.add(2);
            set.add(3);
            set.add(4);
            set.remove(2);

            let result = set.iter().copied().sorted().collect_vec();
            assert_eq!(result, vec![0, 3, 4]);
        }

        #[test]
        fn clear_test() {
            let mut set = LightSet::new(5);
            set.add(0);
            set.add(2);
            set.add(3);
            set.add(4);
            set.remove(2);
            set.clear();

            let result = set.iter().copied().sorted().collect_vec();
            assert_eq!(result, vec![]);
        }

        #[test]
        fn contains_test() {
            let mut set = LightSet::new(5);
            set.add(0);
            set.add(2);
            set.add(3);
            set.add(4);
            set.remove(2);

            assert!(set.contains(0));
            assert!(!set.contains(2));
        }
    }
}

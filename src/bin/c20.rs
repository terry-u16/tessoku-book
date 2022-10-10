use std::time::Instant;

#[allow(unused_imports)]
use proconio::*;
#[allow(unused_imports)]
use rand::prelude::*;

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

fn main() {
    let input = Input::read_input();
    eprintln!("{}", input.map_size);
}

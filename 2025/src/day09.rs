use std::collections::{HashSet, VecDeque};
use std::cmp::max;

pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input =
        std::fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Cannot read {}", filename));

    println!("Day 09 – Part 1: {}", part1(&input));
    println!("Day 09 – Part 2: {}", part2(&input));
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Vec2 {
    x: i64,
    y: i64,
}

fn part1(input: &str) -> i64 {
    let parsed_input: Vec<Vec2> = input.lines()
        .map(|line| {
            let mut parts = line.split(',');
            Vec2 {
                x: parts.next().unwrap().parse().unwrap(),
                y: parts.next().unwrap().parse().unwrap(),
            }
        })
        .collect();

    let mut max_area = 0;
    for vec in &parsed_input {
        for vec2 in &parsed_input {
            let distance_x = (vec.x - vec2.x).abs() + 1;
            let distance_y = (vec.y - vec2.y).abs() + 1;
            let area = distance_x * distance_y;

            max_area = max(area, max_area)
        }
    }

    max_area
}

fn area(a: Vec2, b: Vec2) -> i64 {
    (1 + (a.x - b.x).abs()) * (1 + (a.y - b.y).abs())
}

fn intersects(a: Vec2, b: Vec2, poly: &[Vec2]) -> bool {
    let x_min = a.x.min(b.x);
    let x_max = a.x.max(b.x);
    let y_min = a.y.min(b.y);
    let y_max = a.y.max(b.y);

    !poly.windows(2)
        .chain(std::iter::once(&[poly[poly.len() - 1], poly[0]][..]))
        .all(|w| {
            let (p1, p2) = (w[0], w[1]);
            let lx_min = p1.x.min(p2.x);
            let lx_max = p1.x.max(p2.x);
            let ly_min = p1.y.min(p2.y);
            let ly_max = p1.y.max(p2.y);

            lx_max <= x_min || lx_min >= x_max || ly_max <= y_min || ly_min >= y_max
        })
}

fn part2(input: &str) -> i64 {
    let poly: Vec<Vec2> = input.lines()
        .map(|line| {
            let mut parts = line.split(',');
            Vec2 {
                x: parts.next().unwrap().parse().unwrap(),
                y: parts.next().unwrap().parse().unwrap(),
            }
        })
        .collect();

    let mut max_area = 0;
    for &p in poly.iter() {
        for &p2 in poly.iter() {
            if !intersects(p, p2, &poly) {
                let a = area(p, p2);
                if a > max_area {
                    max_area = a;
                }
            }
        }
    }
    max_area
}


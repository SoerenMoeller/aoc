#[derive(Debug)]
struct Range {
    start: i64,
    end: i64,
}

pub fn run() {
    let input = std::fs::read_to_string("inputs/day02.txt").unwrap();
    let parsed: Vec<Range> = input.split(',').map(parse_range).collect();
    println!("Day 02 – Part 1: {}", part1(&parsed));
    println!("Day 02 – Part 2: {}", part2(&parsed));
}

fn parse_range(s: &str) -> Range {
    let parts = s.trim().split('-').collect::<Vec<&str>>();
    Range {
        start: parts[0].parse::<i64>().unwrap(),
        end: parts[1].parse::<i64>().unwrap(),
    }
}

fn is_invalid_a(num: i64) -> bool {
    let length = num.checked_ilog10().unwrap_or(0) + 1;

    let num_front = num / 10_i64.pow(length / 2);
    let num_back = num % 10_i64.pow(length / 2);

    num_front == num_back
}

fn is_invalid_b(num: i64) -> bool {
    let num_str = num.to_string();
    let length = num_str.chars().count();
    let len_divisors = (1..=length / 2).filter(|&d| length.is_multiple_of(d));

    for d in len_divisors {
        let short_number_str = &num_str[0..d];
        let repeats = length / d;
        let long_number_str = short_number_str.repeat(repeats);

        if long_number_str == num_str {
            return true;
        }
    }
    false
}

fn part1(input: &[Range]) -> i64 {
    input
        .iter()
        .flat_map(|range| range.start..=range.end)
        .filter(|&num| is_invalid_a(num))
        .sum::<i64>()
}

fn part2(input: &[Range]) -> i64 {
    input
        .iter()
        .flat_map(|range| range.start..=range.end)
        .filter(|&num| is_invalid_b(num))
        .sum::<i64>()
}

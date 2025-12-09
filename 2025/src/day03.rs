pub fn run() {
    let input = std::fs::read_to_string("inputs/day03.txt").unwrap();
    let parsed_input = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as i64)
                .collect::<Vec<i64>>()
        })
        .collect::<Vec<Vec<i64>>>();

    println!("Day 03 – Part 1: {}", part1(&parsed_input));
    println!("Day 03 – Part 2: {}", part2(&parsed_input));
}

fn max_index(nums: &[i64], from: usize, to: usize) -> usize {
    let mut best_i = from;
    let mut best = nums[from];

    for i in from + 1..to {
        if nums[i] <= best {
            continue;
        }

        best = nums[i];
        best_i = i;
    }

    best_i
}

fn evaluate_row(nums: &[i64], amount: usize) -> i64 {
    let mut start_idx: usize = 0;
    let mut result: i64 = 0;
    for i in 0..amount {
        let max_idx: usize = nums.len() - (amount - i - 1);
        let max_i: usize = max_index(nums, start_idx, max_idx);

        result *= 10;
        result += nums[max_i];
        start_idx = max_i + 1;
    }

    result
}

fn part1(rows: &[Vec<i64>]) -> i64 {
    rows.iter().map(|nums| evaluate_row(nums, 2)).sum()
}

fn part2(rows: &[Vec<i64>]) -> i64 {
    rows.iter().map(|nums| evaluate_row(nums, 12)).sum()
}

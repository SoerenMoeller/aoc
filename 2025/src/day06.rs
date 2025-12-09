pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input =
        std::fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Cannot read {}", filename));

    println!("Day 06 – Part 1: {}", part1(&input));
    println!("Day 06 – Part 2: {}", part2(&input));
}

fn part1(input: &str) -> i64 {
    let parsed_input = input
        .lines()
        .map(|line| line.split_whitespace().collect::<Vec<&str>>())
        .collect::<Vec<Vec<&str>>>();

    let (nums, ops) = match parsed_input.as_slice() {
        [nums @ .., ops] => (nums, ops),
        _ => panic!("Input must have at least one line"),
    };

    ops.iter()
        .enumerate()
        .map(|(col, op)| {
            let column_values = nums.iter().map(|row| row[col].parse::<i64>().unwrap());

            match *op {
                "*" => column_values.product::<i64>(),
                "+" => column_values.sum::<i64>(),
                _ => panic!("Unknown op {}", op),
            }
        })
        .sum()
}

fn split_at_lengths(s: &str, lengths: &[usize]) -> Vec<String> {
    let mut start = 0;
    lengths
        .iter()
        .map(|&len| {
            let end = start + len;
            let slice = &s[start..end];
            start = end + 1;
            slice.to_string()
        })
        .collect()
}

fn part2(input: &str) -> i64 {
    let binding = input.lines().collect::<Vec<&str>>();
    let (nums_lines, op_line) = match binding.as_slice() {
        [nums @ .., ops] => (nums, ops),
        _ => panic!("Input must have at least one line"),
    };

    let mut nums_lengths = Vec::new();
    for char in op_line.chars() {
        match char {
            '*' | '+' => nums_lengths.push(0),
            ' ' => nums_lengths
                .last_mut()
                .map(|x| *x += 1)
                .expect("Line cannot start with space"),
            _ => panic!("Unknown character {}", char),
        }
    }
    nums_lengths.last_mut().map(|x| *x += 1);

    let nums_lines2: Vec<Vec<String>> = nums_lines
        .iter()
        .map(|s| split_at_lengths(s, &nums_lengths))
        .collect();

    let ops: Vec<&str> = op_line.split_whitespace().collect();

    ops.iter()
        .enumerate()
        .map(|(col, op)| {
            let nums = (0..nums_lengths[col]).map(|i| {
                nums_lines2
                    .iter()
                    .map(|row| row[col].chars().nth(i).unwrap())
                    .filter(|c| !c.is_whitespace())
                    .collect::<String>()
                    .parse::<i64>()
                    .unwrap()
            });

            match *op {
                "*" => nums.product::<i64>(),
                "+" => nums.sum::<i64>(),
                _ => panic!("Unknown op {}", op),
            }
        })
        .sum::<i64>()
}

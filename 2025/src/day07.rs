pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input =
        std::fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Cannot read {}", filename));

    println!("Day 07 – Part 1: {}", part1(&input));
    println!("Day 07 – Part 2: {}", part2(&input));
}

fn part1(input: &str) -> usize {
    let interesting_lines: Vec<Vec<char>> = input
        .lines()
        .skip(2)
        .step_by(2)
        .map(|line| line.chars().collect())
        .collect();

    let line_width: usize = interesting_lines[0].len();
    let mut positions: Vec<usize> = vec![line_width / 2];
    let mut splits = 0;

    for line in &interesting_lines {
        splits += positions.iter().filter(|&&i| line[i] == '^').count();

        positions = (0..line_width)
            .filter(|&i| {
                line[i] == '.' && positions.contains(&i)
                    || i > 0 && line[i - 1] == '^' && positions.contains(&(i - 1))
                    || i + 1 < line_width && line[i + 1] == '^' && positions.contains(&(i + 1))
            })
            .collect();
    }

    splits
}

fn part2(input: &str) -> usize {
    let interesting_lines: Vec<Vec<char>> = input
        .lines()
        .skip(2)
        .step_by(2)
        .map(|line| line.chars().collect())
        .collect();

    let line_width: usize = interesting_lines[0].len();
    let mut positions: Vec<usize> = vec![0; line_width];
    positions[line_width / 2] = 1;

    for line in &interesting_lines {
        positions = (0..line_width)
            .map(|i| {
                let mut count = 0;

                if line[i] == '.' {
                    count += positions[i];
                }

                if i > 0 && line[i - 1] == '^' {
                    count += positions[i - 1];
                }

                if i + 1 < line_width && line[i + 1] == '^' {
                    count += positions[i + 1];
                }

                count
            })
            .collect();
    }

    positions.iter().sum()
}

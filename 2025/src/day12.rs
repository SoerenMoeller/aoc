use regex::Regex;

pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input =
        std::fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Cannot read {}", filename));

    let shapes = parse_shapes(&input);
    let spaces = parse_spaces(&input);
    
    println!("Day 12 – Part 1: {}", part1(&shapes, &spaces));
}

#[derive(Debug)]
struct Shape {
    index: i64,
    grid: Vec<Vec<bool>>,
}

fn parse_shapes(input: &str) -> Vec<Shape> {
    let re = Regex::new(r"(?m)^(\d+):\n((?:[.#]+\n)+)").unwrap();

    re.captures_iter(input) 
        .map(|cap| {
            let index: i64 = cap[1].parse().unwrap();
            let shape: Vec<Vec<bool>> = cap[2]
                .lines()
                .map(|line| {
                    line.chars().map(|c| match c {
                        '#' => true,
                        '.' => false,
                        _ => panic!("Unexpected character in shape"),
                    })
                    .collect()
                })
                .collect();
        
            Shape { index, grid: shape }
        })
        .collect()
}

#[derive(Debug)]
struct SpaceRequirements {
    height: i64,
    width: i64,
    values: Vec<i64>,
}

fn parse_spaces(input: &str) -> Vec<SpaceRequirements> {
    let re = Regex::new(r"(?m)^(\d+)x(\d+):\s*(.*)$").unwrap();

    re.captures_iter(input)
        .map(|cap| {
            let w = cap[1].parse::<i64>().unwrap();
            let h = cap[2].parse::<i64>().unwrap();

            let values = cap[3]
                .split_whitespace()
                .map(|s| s.parse::<i64>().unwrap())
                .collect::<Vec<_>>();

            SpaceRequirements { height: h, width: w, values }
        })
        .collect()
}

fn part1(shapes: &[Shape], spaces: &[SpaceRequirements]) -> i64 {
    spaces
        .iter()
        .filter(|space| {
            space.values.iter().sum::<i64>() <= (space.height / 3 * space.width / 3) 
        })
        .count() as i64
}


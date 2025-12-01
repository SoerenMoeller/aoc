use regex::Regex;

pub fn run() {
    let input = std::fs::read_to_string("inputs/day01.txt").unwrap();
    println!("Day 01 – Part 1: {}", part1(&input));
    println!("Day 01 – Part 2: {}", part2(&input));
}

fn parse_line(line: &str) -> i64 {
    let re = Regex::new(r"([LR])(\d+)").unwrap();
    let cap = re.captures(line).expect("No L/R+number match found");

    let dir = cap[1].chars().next().unwrap();       // 'L' or 'R'
    let num: i64 = cap[2].parse().expect("Failed to parse number");

    return match dir {
        'L' => -num,
        'R' => num,
        _ => panic!("Unexpected direction"),
    };
}

fn part1(input: &str) -> usize {
    return input.lines()
        .map(|line| parse_line(line))   
        .scan(50, |acc, x| {
            *acc = (*acc + x) % 100;   
            Some(*acc)                
        })
        .filter(|&x| x == 0)         
        .count();                    
}

fn part2(input: &str) -> i64 {
    let parsed_input = input.lines().map(|line| parse_line(line)).collect::<Vec<i64>>();
    
    let mut value: i64 = 50;
    let mut count: i64 = 0;
    for num in parsed_input {
        let raw = value + num;

        if raw >= 100 || raw <= 0 {
            count += raw.abs() / 100; 
        }

        if value != 0 && raw <= 0 {
            count += 1; 
        }

        value = raw.rem_euclid(100);
    } 
    return count;
}


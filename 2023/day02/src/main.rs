use regex::Regex;
use std::fs;

#[derive(Debug)]
struct Game {
    id: u32,
    rounds: Vec<Round>,
}

#[derive(Debug)]
struct Used {
    id: u32, 
    used: Round,
}

#[derive(Debug)]
struct Round {
    blue: u32,
    green: u32,
    red: u32,
}


fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input");

    println!("Part 1: {}", part1(&input));
    println!("Part 1: {}", part2(&input));
}

fn part1(input: &str) -> u32 {
    let games: u32 = input 
        .lines()
        .map(|line| parse_game(line))
        .filter(|used| 
            used.rounds.iter().all(|round| round.red <= 12 && round.green <= 13 && round.blue <= 14)
        )
        .map(|used| used.id)
        .sum::<u32>();

    games 
}

fn part2(input: &str) -> u32 {
    let games: u32 = input 
        .lines()
        .map(|line| parse_game(line))
        .map(|used| 
            used.rounds.iter().map(|round| round.red).max().unwrap() *
            used.rounds.iter().map(|round| round.green).max().unwrap() *
            used.rounds.iter().map(|round| round.blue).max().unwrap()
        )
        .sum::<u32>();

    games
}

fn parse_game(line: &str) -> Game {
    let game_re = Regex::new(r"Game (\d+): (.+)").unwrap();
    let caps = game_re.captures(line).unwrap();
    let game_number: u32 = caps[1].parse().unwrap();

    let rounds = &caps[2];
    let num_re = Regex::new(r"(\d+) (\w+)").unwrap();
    let rounds_final = rounds 
        .split(";")
        .map(|round| {
            let mut blue = 0;
            let mut green = 0;
            let mut red = 0;
            
            for c in num_re.captures_iter(round) {
                let val: u32 = c[1].parse().unwrap();
                match &c[2] {
                    "blue" => blue = val,
                    "green" => green = val,
                    "red"  => red = val,
                    _ => unreachable!(),
                }
            }
            Round {blue, green, red}
        })
        .collect();

    Game{id: game_number, rounds: rounds_final}
}

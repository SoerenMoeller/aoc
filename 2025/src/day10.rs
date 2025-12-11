use std::collections::HashMap;
use z3::{ast::Int, Optimize, SatResult};

pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input =
        std::fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Cannot read {}", filename));

    let parsed_machines = parse_machines(&input);

    println!("Day 10 – Part 1: {}", part1(&parsed_machines));
    println!("Day 10 – Part 2: {}", part2(&parsed_machines));
}

#[derive(Debug)]
struct Machine {
    indicator_goal: Vec<bool>,
    button_lists: Vec<Vec<usize>>,
    joltage_goal: Vec<usize>,
}

fn parse_machines(input: &str) -> Vec<Machine> {
    input
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            let indicator_goal: Vec<bool> = parts[0]
                .get(1..parts[0].len() - 1)
                .unwrap()
                .chars()
                .map(|c| c == '#')
                .collect();

            let joltage_goal = parts[parts.len() - 1][1..parts[parts.len() - 1].len() - 1]
                .split(',')
                .map(|s| s.parse::<usize>().unwrap())
                .collect();

            let button_lists: Vec<Vec<usize>> = parts[1..parts.len() - 1]
                .iter()
                .map(|s| {
                    s.get(1..s.len() - 1)
                        .unwrap()
                        .split(',')
                        .map(|num| num.parse::<usize>().unwrap())
                        .collect()
                })
                .collect();

            Machine {
                indicator_goal,
                button_lists,
                joltage_goal,
            }
        })
        .collect()
}

fn fewest_presses(machine: &Machine) -> usize {
    let mut reachable_states: HashMap<Vec<bool>, usize> = HashMap::new();
    let mut to_search = vec![vec![false; machine.indicator_goal.len()]];
    reachable_states.insert(vec![false; machine.indicator_goal.len()], 0);

    let mut button_presses = 0;
    while !to_search.is_empty() {
        let mut new_to_search = vec![];
        button_presses += 1;

        for state in &to_search {
            for button in &machine.button_lists {
                let mut new_state = state.clone();
                for &light in button {
                    new_state[light] = !new_state[light];
                }

                if reachable_states.contains_key(&new_state[..]) {
                    continue;
                }
                reachable_states.insert(new_state.clone(), button_presses);
                new_to_search.push(new_state);
            }
        }

        to_search = new_to_search;
    }

    reachable_states
        .get(&machine.indicator_goal[..])
        .cloned()
        .unwrap()
}

fn part1(machines: &[Machine]) -> usize {
    machines.iter().map(fewest_presses).sum()
}

fn fewest_presses_z3(machine: &Machine) -> Option<usize> {
    // I solve Ax = b with integer linear programming, relying on z3 here
    let opt = Optimize::new();

    let n = machine.button_lists.len();
    let m = machine.joltage_goal.len();

    let xs: Vec<Int> = (0..n)
        .map(|j| {
            let name = format!("x{}", j);
            Int::new_const(name.as_str())
        })
        .collect();

    for x in &xs {
        opt.assert(&x.ge(Int::from_i64(0)));
    }

    for i in 0..m {
        let mut terms = vec![];
        for j in 0..n {
            if machine.button_lists[j].contains(&i) {
                terms.push(&xs[j]);
            }
        }
        let sum = Int::add(&terms);
        let target = Int::from_i64(machine.joltage_goal[i] as i64);
        opt.assert(&sum.eq(&target));
    }

    let total = Int::add(&xs.iter().collect::<Vec<_>>());
    opt.minimize(&total);

    match opt.check(&[]) {
        SatResult::Sat => {
            let model = opt.get_model().unwrap();
            model
                .eval(&total, true)
                .and_then(|v| v.as_i64())
                .map(|v| v as usize)
        }
        SatResult::Unsat | SatResult::Unknown => None,
    }
}

fn part2(machines: &[Machine]) -> usize {
    machines.iter().map(|x| fewest_presses_z3(x).unwrap()).sum()
}

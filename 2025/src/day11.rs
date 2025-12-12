use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub fn run(day: u32, ex: bool) {
    let filename_a = if ex {
        format!("inputs/day{:02}-example-a.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };
    let filename_b = if ex {
        format!("inputs/day{:02}-example-b.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input_a = std::fs::read_to_string(&filename_a)
        .unwrap_or_else(|_| panic!("Cannot read {}", filename_a));
    let input_b = std::fs::read_to_string(&filename_b)
        .unwrap_or_else(|_| panic!("Cannot read {}", filename_b));

    let graph_a = get_graph(&input_a);
    let graph_b = get_graph(&input_b);

    println!("Day 11 – Part 1: {}", part1(&graph_a));
    println!("Day 11 – Part 2: {}", part2(&graph_b));
}

fn get_graph(input: &str) -> HashMap<&str, HashSet<&str>> {
    let mut graph: HashMap<&str, HashSet<&str>> = HashMap::new();
    input.lines().for_each(|line| {
        let parts: Vec<&str> = line.split(":").collect();
        let from: &str = parts[0].trim();
        let tos: HashSet<&str> = parts[1].split_whitespace().map(|s| s.trim()).collect();

        graph.insert(from, tos);
    });
    graph
}

fn part1(graph: &HashMap<&str, HashSet<&str>>) -> u32 {
    let start = "you";
    let end = "out";

    let mut reachable_amount: HashMap<&str, u32> = HashMap::new();
    reachable_amount.insert(start, 1);

    let mut queue: VecDeque<&str> = VecDeque::new();
    queue.push_back(start);

    while let Some(current) = queue.pop_front() {
        if let Some(neighbors) = graph.get(current) {
            let current_count = reachable_amount[&current];

            for &neighbor in neighbors {
                if !reachable_amount.contains_key(&neighbor) {
                    queue.push_back(neighbor);
                }

                let entry = reachable_amount.entry(neighbor).or_insert(0);
                *entry += current_count;
            }
        }
    }

    reachable_amount[end]
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum VisitedState {
    None,
    Node1,
    Node2,
    Both,
}

impl VisitedState {
    fn add_node1(self) -> Self {
        match self {
            VisitedState::None => VisitedState::Node1,
            VisitedState::Node2 => VisitedState::Both,
            _ => self,
        }
    }

    fn add_node2(self) -> Self {
        match self {
            VisitedState::None => VisitedState::Node2,
            VisitedState::Node1 => VisitedState::Both,
            _ => self,
        }
    }

    fn is_both(self) -> bool {
        matches!(self, VisitedState::Both)
    }
}

fn count_paths(
    graph: &HashMap<&str, HashSet<&str>>,
    start: &str,
    end: &str,
    node1: &str,
    node2: &str,
) -> usize {
    let mut memo: HashMap<(String, VisitedState), usize> = HashMap::new();

    fn dfs(
        graph: &HashMap<&str, HashSet<&str>>,
        current: &str,
        end: &str,
        node1: &str,
        node2: &str,
        state: VisitedState,
        memo: &mut HashMap<(String, VisitedState), usize>,
    ) -> usize {
        let key = (current.to_string(), state);
        if let Some(&count) = memo.get(&key) {
            return count;
        }

        let mut new_state = state;
        if current == node1 {
            new_state = new_state.add_node1();
        }
        if current == node2 {
            new_state = new_state.add_node2();
        }

        if current == end {
            let count = if new_state.is_both() { 1 } else { 0 };
            memo.insert(key, count);
            return count;
        }

        let mut total = 0;
        if let Some(neighbors) = graph.get(current) {
            for neighbor in neighbors {
                total += dfs(graph, neighbor, end, node1, node2, new_state, memo);
            }
        }

        memo.insert(key, total);
        total
    }

    dfs(
        graph,
        start,
        end,
        node1,
        node2,
        VisitedState::None,
        &mut memo,
    )
}

fn part2(graph: &HashMap<&str, HashSet<&str>>) -> usize {
    count_paths(graph, "svr", "out", "fft", "dac")
}

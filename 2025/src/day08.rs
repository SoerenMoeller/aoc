use std::collections::HashSet;

pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let amount = if ex { 10 } else { 1000 };

    let input =
        std::fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Cannot read {}", filename));

    println!("Day 08 – Part 1: {}", part1(&input, amount));
    println!("Day 08 – Part 2: {}", part2(&input));
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Vec3 {
    x: i64,
    y: i64,
    z: i64,
}

fn parse_input(input: &str) -> Vec<Vec3> {
    input
        .lines()
        .map(|line| {
            let coords: Vec<i64> = line
                .split(',')
                .map(|num| num.parse::<i64>().unwrap())
                .collect();
            Vec3 {
                x: coords[0],
                y: coords[1],
                z: coords[2],
            }
        })
        .collect()
}

fn euclidean_distance(a: &Vec3, b: &Vec3) -> f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    let dz = a.z - b.z;
    ((dx * dx + dy * dy + dz * dz) as f64).sqrt()
}

fn distance_ordered_pairs(vectors: &[Vec3]) -> Vec<(Vec3, Vec3)> {
    let mut pairs = Vec::new();

    for i in 0..vectors.len() {
        for j in (i + 1)..vectors.len() {
            pairs.push((vectors[i].clone(), vectors[j].clone()));
        }
    }

    pairs.sort_by_key(|(a, b)| {
        let dist = euclidean_distance(a, b);
        (dist * 1000.0) as i64
    });

    pairs
}

fn build_boxes(pairs: &[(Vec3, Vec3)], amount: usize) -> Vec<HashSet<Vec3>> {
    let mut boxes: Vec<HashSet<Vec3>> = Vec::new();

    (0..amount).for_each(|i| {
        let (p1, p2) = &pairs[i];

        let mut index_p1: isize = -1;
        let mut index_p2: isize = -1;
        for (idx, bx) in boxes.iter().enumerate() {
            if bx.contains(p1) {
                index_p1 = idx as isize;
            }
            if bx.contains(p2) {
                index_p2 = idx as isize;
            }
        }

        match (index_p1, index_p2) {
            (-1, -1) => {
                boxes.push(HashSet::from([p1.clone(), p2.clone()]));
            }
            (i1, -1) => {
                boxes[i1 as usize].insert(p2.clone());
            }
            (-1, i2) => {
                boxes[i2 as usize].insert(p1.clone());
            }
            (i1, i2) if i1 == i2 => {}
            (i1, i2) => {
                let (i1, i2) = (i1 as usize, i2 as usize);

                let (big, small) = if i1 < i2 { (i1, i2) } else { (i2, i1) };
                let removed = boxes.remove(small);
                boxes[big].extend(removed);
            }
        }
    });

    boxes
}

fn part1(input: &str, amount: usize) -> usize {
    let parsed_input = parse_input(input);
    let pairs = distance_ordered_pairs(&parsed_input);

    let mut boxes: Vec<HashSet<Vec3>> = build_boxes(&pairs, amount);
    boxes.sort_by_key(|b| std::cmp::Reverse(b.len()));

    boxes[0].len() * boxes[1].len() * boxes[2].len()
}

fn find_last_connection(pairs: &[(Vec3, Vec3)], total_nodes: usize) -> Option<(Vec3, Vec3)> {
    let mut boxes: Vec<HashSet<Vec3>> = Vec::new();

    for (p1, p2) in pairs.iter() {
        let mut index_p1: isize = -1;
        let mut index_p2: isize = -1;
        for (idx, bx) in boxes.iter().enumerate() {
            if bx.contains(p1) {
                index_p1 = idx as isize;
            }
            if bx.contains(p2) {
                index_p2 = idx as isize;
            }
        }

        match (index_p1, index_p2) {
            (-1, -1) => {
                boxes.push(HashSet::from([p1.clone(), p2.clone()]));
            }
            (i1, -1) => {
                boxes[i1 as usize].insert(p2.clone());
            }
            (-1, i2) => {
                boxes[i2 as usize].insert(p1.clone());
            }
            (i1, i2) if i1 == i2 => {}
            (i1, i2) => {
                let (i1, i2) = (i1 as usize, i2 as usize);

                let (big, small) = if i1 < i2 { (i1, i2) } else { (i2, i1) };
                let removed = boxes.remove(small);
                boxes[big].extend(removed);
            }
        }

        if boxes.len() == 1 && boxes[0].len() == total_nodes {
            return Some((p1.clone(), p2.clone()));
        }
    }

    None
}

fn part2(input: &str) -> i64 {
    let parsed_input = parse_input(input);
    let pairs = distance_ordered_pairs(&parsed_input);

    match find_last_connection(&pairs, parsed_input.len()) {
        Some((vec1, vec2)) => vec1.x * vec2.x,
        None => 0,
    }
}

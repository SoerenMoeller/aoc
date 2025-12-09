pub fn run(day: u32, ex: bool) {
    let filename = if ex {
        format!("inputs/day{:02}-example.txt", day)
    } else {
        format!("inputs/day{:02}.txt", day)
    };

    let input = std::fs::read_to_string(&filename).unwrap();
    let (block1, block2) = input.split_once("\n\n").unwrap();

    let ranges = block1
        .lines()
        .map(|line| {
            let (start, end) = line.split_once('-').unwrap();
            Range {
                start: start.parse().unwrap(),
                end: end.parse().unwrap(),
            }
        })
        .collect::<Vec<Range>>();

    let numbers_to_check = block2
        .lines()
        .map(|line| line.parse::<u64>().unwrap())
        .collect::<Vec<u64>>();

    let parsed_input = (ranges, numbers_to_check);

    println!("Day 05 – Part 1: {}", part1(&parsed_input));
    println!("Day 05 – Part 2: {}", part2(&parsed_input));
}

#[derive(Clone)]
struct Range {
    start: u64,
    end: u64,
}

fn is_in_range(value: u64, range: &Range) -> bool {
    value >= range.start && value <= range.end
}

fn part1((ranges, numbers_to_check): &(Vec<Range>, Vec<u64>)) -> u64 {
    numbers_to_check
        .iter()
        .filter(|&&number| ranges.iter().any(|range| is_in_range(number, range)))
        .count() as u64
}

fn part2((ranges, _numbers_to_check): &(Vec<Range>, Vec<u64>)) -> u64 {
    let mut sorted_ranges = ranges.clone();
    sorted_ranges.sort_by_key(|r| r.start);

    let mut new_ranges: Vec<Range> = Vec::new();
    let mut current_range: Range = Range {
        start: sorted_ranges[0].start,
        end: sorted_ranges[0].end,
    };

    for range in sorted_ranges {
        if range.start <= current_range.end + 1 {
            current_range.end = current_range.end.max(range.end);
            continue;
        }
        new_ranges.push(current_range);
        current_range = Range {
            start: range.start,
            end: range.end,
        };
    }
    new_ranges.push(current_range);

    new_ranges
        .iter()
        .map(|range| range.end - range.start + 1)
        .sum()
}

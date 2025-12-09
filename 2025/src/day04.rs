pub fn run() {
    let input = std::fs::read_to_string("inputs/day04.txt").unwrap();
    let mut parsed_input = input.lines()    
        .map(|line| line.chars()
            .map(|c| match c {
                '.' => Cell::Empty,
                '@' => Cell::Paperroll,
                _ => panic!("Unknown cell type"),
            })
            .collect::<Vec<Cell>>()
        )
        .collect::<Vec<Vec<Cell>>>();
    println!("Day 04 – Part 1: {}", part1(&parsed_input));
    println!("Day 04 – Part 2: {}", part2(&mut parsed_input));
}

#[derive(Debug)]
enum Cell {
    Empty,
    Paperroll
}

fn is_accessible(grid: &[Vec<Cell>], x: usize, y: usize) -> bool {
    let cell = &grid[y][x];
    
    if !matches!(cell, Cell::Paperroll) {
        return false;
    }

    let neighbor_offsets: [(isize, isize); 8] = [
        (-1, -1), (0, -1), (1, -1),
        (-1, 0),           (1, 0),
        (-1, 1),  (0, 1),  (1, 1),
    ];
    
    let neighbors = neighbor_offsets
        .iter()
        .map(|(dx, dy)| (x as isize + dx, y as isize + dy))
        .filter(|(nx, ny)| {
            *nx >= 0
                && *ny >= 0
                && *nx < grid.len() as isize
                && *ny < grid[0].len() as isize
                && matches!(grid[*ny as usize][*nx as usize], Cell::Paperroll)
        });

    neighbors.count() < 4
}

fn removable_cells(grid: &[Vec<Cell>]) -> Vec<(usize, usize)> {
    (0..grid.len()*grid[0].len())
        .map(|i| (i % grid[0].len(), i / grid[0].len()))
        .filter(|(x, y)| is_accessible(grid, *x, *y))
        .collect::<Vec<(usize, usize)>>()
}

fn part1(grid: &[Vec<Cell>]) -> usize {
    removable_cells(grid).len() 
}

fn part2(grid: &mut [Vec<Cell>]) -> usize {
    let mut count = 0;
    
    loop {
        let to_remove = removable_cells(grid);
        for &(x, y) in &to_remove {
            grid[y][x] = Cell::Empty;
        } 

        count += to_remove.len();

        if to_remove.is_empty() {
            break;
        }
    }

    count
}


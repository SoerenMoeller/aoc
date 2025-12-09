// imports
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;

fn main() {
    let mut args = std::env::args().skip(1);

    let mut day: Option<u32> = None;
    let mut ex = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--ex" => ex = true,
            _ => {
                // Assume the first non-flag argument is `day`
                day = Some(arg.parse::<u32>().expect("Day must be a number"));
            }
        }
    }

    let day = day.expect("Pass a day number, e.g., `cargo run -- 1`");

    match day {
        // days
        8 => day08::run(day, ex),
        7 => day07::run(day, ex),
        6 => day06::run(day, ex),
        5 => day05::run(day, ex),
        4 => day04::run(),
        3 => day03::run(),
        2 => day02::run(),
        1 => day01::run(),
        _ => panic!("Day not implemented"),
    }
}

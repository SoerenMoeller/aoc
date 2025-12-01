// imports
mod day01;

fn main() {
    let day = std::env::args()
        .nth(1)
        .expect("Pass a day number, e.g., `cargo run -- 1`")
        .parse::<u32>()
        .expect("Day must be a number");

    match day {
        // days
        1 => day01::run(),
        _ => panic!("Day not implemented"),
    }
}

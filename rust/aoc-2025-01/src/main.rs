use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::path;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<fs::File>>>
where
    P: AsRef<path::Path>,
{
    let file = fs::File::open(filename)?;
    io::Result::Ok(io::BufReader::new(file).lines())
}

fn part1(lines: Vec<String>) -> i32 {
    lines
        .into_iter()
        .fold((50, 0), |(clk, code), step_in| {
            let mut step = step_in.clone();
            let clicks: i32 = step.split_off(1).parse().unwrap_or(0);
            let direction = if step == "L" { -1 } else { 1 };
            let new_clicks = (clk + direction * clicks).rem_euclid(100);
            (new_clicks, code + (if new_clicks == 0 { 1 } else { 0 }))
        })
        .1
}

fn part2(lines: Vec<String>) -> i32 {
    lines
        .into_iter()
        .fold((50, 0), |(clk, code), step_in| {
            let mut step = step_in.clone();
            let clicks: i32 = step.split_off(1).parse().unwrap_or(0);
            let direction = if step == "L" { -1 } else { 1 };
            let new_clicks = clk + direction * clicks;
            let new_pos = new_clicks.rem_euclid(100);
            (
                new_pos,
                code + new_clicks.abs() / 100 + (if clk > 0 && new_clicks <= 0 { 1 } else { 0 }),
            )
        })
        .1
}

fn main() {
    let input_rel_path = env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("./input"));

    let result = path::absolute(input_rel_path)
        .and_then(|input_path| {
            read_lines(input_path).map(|lines| lines.map_while(io::Result::ok).collect())
        })
        .map(|lines: Vec<String>| (part1(lines.clone()), part2(lines)));

    match result {
        io::Result::Ok((x, y)) => println!("{x} - {y}"),
        io::Result::Err(err) => println!("something is wrong!\n{0}", err),
    }
}

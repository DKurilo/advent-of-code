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

fn part1(lines: Vec<String>) -> String {
    lines
        .into_iter()
        .fold("1".to_string(), |acc, x| format!("{acc}{x}"))
}

fn part2(lines: Vec<String>) -> String {
    lines
        .into_iter()
        .fold("2".to_string(), |acc, x| format!("{acc}{x}"))
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

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

fn part1(bs: Vec<Vec<i64>>) -> i64 {
    bs.into_iter().map(|bat_row| max_power(2i64, bat_row)).sum()
}

fn part2(bs: Vec<Vec<i64>>) -> i64 {
    bs.into_iter()
        .map(|bat_row| max_power(12i64, bat_row))
        .sum()
}

fn parse_powerbank(line: String) -> Vec<i64> {
    line.split("").map(|c| c.parse().unwrap_or(0)).collect()
}

fn max_power(n: i64, xs: Vec<i64>) -> i64 {
    if n == 0 {
        0
    } else {
        let l = xs.len();
        let to_take = l - (n as usize);
        let max = xs.iter().take(to_take).cloned().max().unwrap_or(0i64);
        let mut xiter = xs.iter().cloned();
        let mut x = xiter.next();
        while x.unwrap_or(max) != max {
            x = xiter.next();
        }
        let rest_xs: Vec<i64> = xiter.collect();
        10i64.pow((n - 1).try_into().unwrap()) * max + max_power(n - 1, rest_xs)
    }
}

fn main() {
    let input_rel_path = env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("./input"));

    let result = path::absolute(input_rel_path)
        .and_then(|input_path| {
            read_lines(input_path).map(|lines| lines.map_while(io::Result::ok).collect())
        })
        .map(|lines: Vec<String>| {
            let bs: Vec<Vec<i64>> = lines.iter().cloned().map(parse_powerbank).collect();
            (part1(bs.clone()), part2(bs))
        });

    match result {
        io::Result::Ok((x, y)) => println!("{x} - {y}"),
        io::Result::Err(err) => println!("something is wrong!\n{0}", err),
    }
}

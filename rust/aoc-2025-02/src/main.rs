use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Error;
use std::path;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<fs::File>>>
where
    P: AsRef<path::Path>,
{
    let file = fs::File::open(filename)?;
    io::Result::Ok(io::BufReader::new(file).lines())
}

fn part1(ranges: &Vec<(i64, i64)>) -> i64 {
    ranges
        .iter()
        .map(|(x, y)| {
            (x.clone()..=y.clone())
                .filter(|n| {
                    let l2: i64 = (n.to_string().len() as i64) / 2;
                    let mul = 10i64.pow(l2.try_into().unwrap());
                    n / mul == n.rem_euclid(mul)
                })
                .sum::<i64>()
        })
        .sum()
}

fn part2(ranges: &Vec<(i64, i64)>) -> i64 {
    ranges
        .iter()
        .map(|(x, y)| {
            (x.clone()..=y.clone())
                .filter(|n| {
                    let l: i64 = n.to_string().len() as i64;
                    for l3 in 1..=(l / 2) {
                        if l.rem_euclid(l3) != 0 {
                            continue;
                        }
                        let mul = 10i64.pow(l3.try_into().unwrap());
                        let mut n1 = n.clone();
                        let mut is_invalid = true;
                        let rem = n1.rem_euclid(mul);
                        while n1 > 0 {
                            if n1.rem_euclid(mul) != rem {
                                is_invalid = false;
                                break;
                            }
                            n1 = n1 / mul;
                        }
                        if is_invalid {
                            return true;
                        }
                    }
                    false
                })
                .sum::<i64>()
        })
        .sum()
}

fn main() {
    let input_rel_path = env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("./input"));

    let result = path::absolute(input_rel_path)
        .and_then(|input_path| {
            read_lines(input_path).and_then(|mut lines| lines.next().ok_or(Error::other("oh no!")))
        })
        .flatten()
        .map(|line| {
            let ranges = line
                .split(",")
                .map(|str_range| {
                    let arr: Vec<i64> = str_range.split("-").map(|n| n.parse()).flatten().collect();
                    if arr.len() == 2 {
                        (arr[0], arr[1])
                    } else {
                        (0, 0)
                    }
                })
                .collect();
            (part1(&ranges), part2(&ranges))
        });

    match result {
        io::Result::Ok((x, y)) => println!("{x} - {y}"),
        io::Result::Err(err) => println!("something is wrong!\n{0}", err),
    }
}

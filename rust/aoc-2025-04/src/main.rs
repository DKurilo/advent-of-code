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

fn around(plan: &Vec<String>, x: i32, y: i32) -> i32 {
    let h = plan.len() as i32;
    let w = plan[0].len() as i32;
    let mut cnt = 0;
    for i in -1..=1 {
        let x1 = x + i;
        if x1 < 0 || x1 > w - 1 {
            continue;
        }
        for j in -1..=1 {
            let y1 = y + j;
            if y1 < 0 || y1 > h - 1 || i == 0 && j == 0 {
                continue;
            }
            if plan[y1 as usize].chars().nth(x1 as usize).unwrap_or('.') == '@' {
                cnt += 1;
            }
        }
    }
    cnt
}

fn part1(input_plan: &Vec<String>) -> i32 {
    let plan = input_plan.clone();
    let h = plan.len() as i32;
    let w = plan[0].len() as i32;
    let mut cnt = 0;
    for x in 0..w {
        for y in 0..h {
            if plan[y as usize].chars().nth(x as usize).unwrap_or('.') == '@'
                && around(&plan, x, y) < 4
            {
                cnt += 1;
            }
        }
    }
    cnt
}

fn part2(input_plan: &Vec<String>) -> i32 {
    let mut old_plan = input_plan.clone();
    let mut plan = input_plan.clone();
    let h = plan.len() as i32;
    let w = plan[0].len() as i32;
    let mut cnt = 0;
    let mut deleted = true;
    while deleted == true {
        deleted = false;
        for x in 0..w {
            for y in 0..h {
                if old_plan[y as usize].chars().nth(x as usize).unwrap_or('.') == '@'
                    && around(&old_plan, x, y) < 4
                {
                    cnt += 1;
                    deleted = true;
                    let x1 = x as usize;
                    plan[y as usize].replace_range(x1..(x1 + 1), ".");
                }
            }
        }
        old_plan = plan.clone()
    }
    cnt
}

fn main() {
    let input_rel_path = env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("./input"));

    let result = path::absolute(input_rel_path)
        .and_then(|input_path| {
            read_lines(input_path).map(|lines| lines.map_while(io::Result::ok).collect())
        })
        .map(|lines: Vec<String>| (part1(&lines), part2(&lines)));

    match result {
        io::Result::Ok((x, y)) => println!("{x} - {y}"),
        io::Result::Err(err) => println!("something is wrong!\n{0}", err),
    }
}

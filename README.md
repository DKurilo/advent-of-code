# [Advent of Code](https://adventofcode.com/)

Includes my repos for [2020](https://github.com/DKurilo/advent-of-code-2020) and [2021](https://github.com/DKurilo/advent-of-code-2021) years.

## New year, new day

I used [stack](https://docs.haskellstack.org/en/stable/) for years before 2023. Starting from 2023 I decided to move to [nix](https://nixos.org/) + [cabal](https://www.haskell.org/cabal/).
This combination is better by many reasons [check this](https://github.com/Gabriella439/haskell-nix/tree/main). And it becomes simple enough to start using it. Especially after I found this great [template](https://github.com/jonascarpay/template-haskell).

So how to use it:
start from `./newday.sh` script. Like this:
```
./newday.sh 2023 01
```

It builds directory `2023/day01` and prepares new template and adds newly created files to repo. After this just use:
`nix build` to build project, `nix deveelop` to start console in the same environment where project is built.
To run program use `./result/bin/day01-exe input-test'.
That's it.

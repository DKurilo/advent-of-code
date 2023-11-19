#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: newday.sh <year> <day>"
    exit 1
fi;

YEAR=$1
DAY=$2

mkdir -p "./${YEAR}"
cp -r aoc-template "./${YEAR}/day${DAY}"
cd "./${YEAR}/day${DAY}" || exit 1
./wizard.sh

git add "../../${YEAR}/day${DAY}"

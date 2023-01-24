#!/usr/bin/env python3


import sys


TREE = '#'
INPUT_FILE = 'd/day03.txt'
FIXTURE_FILE = 'f/day03.txt'
SLOPES = [
    (1, 1),
    (1, 3),
    (1, 5),
    (1, 7),
    (2, 1),
]


def read_map(input_file):
    map_lines = [line.strip() for line in open(input_file)]
    return map_lines


def is_tree(map, rise, run):
    line = map[rise]
    run = run % len(line)
    return line[run] == TREE


def count_trees(map, rise_offset, run_offset):
    rise = run = 0
    tree_count = 0

    while rise < len(map):
        if is_tree(map, rise, run):
            tree_count += 1
        rise += rise_offset
        run += run_offset

    return tree_count

def day03a(input_file, rise_run=None):
    rise_run = rise_run if rise_run is not None else (1, 3)
    map = read_map(input_file)
    return count_trees(map, rise_run[0], rise_run[1])


def day03b(input_file):
    map = read_map(input_file)

    checksum = 1
    for (rise, run) in SLOPES:
        cs = count_trees(map, rise, run)
        print(f'rise, run: {rise}, {run}: {cs}')
        checksum *= cs

    return checksum


def main():
    f = day03b if 'b' in sys.argv else day03a
    input_file = FIXTURE_FILE if 'fixture' in sys.argv else INPUT_FILE

    print(f(input_file))


if __name__ == '__main__':
    main()

# rise, run: 1, 1: 93
# rise, run: 1, 3: 164
# rise, run: 1, 5: 82
# rise, run: 1, 7: 91
# rise, run: 2, 1: 44
# 5007658656

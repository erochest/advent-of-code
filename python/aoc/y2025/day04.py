from collections import namedtuple

from aoc.io import read_sample_file, read_data_file


type point = complex
type grid = dict[point, int]


def to_grid(input: str) -> grid:
    g = {}
    p = 0 + 1j

    for line in input.splitlines():
        p = complex(p.real+1, 1)
        for char in line:
            g[p] = int(char == '@')
            p += 0 + 1j

    return g


def get_neighbors(center: point) -> list[point]:
    return [
        center + (-1-1j),
        center + ( 0-1j),
        center + (+1-1j),
        center + (-1+0j),
        center + (+1+0j),
        center + (-1+1j),
        center + ( 0+1j),
        center + (+1+1j),
    ]


def remove_accessible(input: grid) -> tuple[grid, int]:
    """Returns the number of accessible squares"""
    cleaned = input.copy()
    changed_count = 0

    for (current, value) in input.items():
        if value == 0:
            continue
        neighbors = get_neighbors(current)
        count = sum(input.get(p, 0) for p in neighbors)
        if count < 4:
            cleaned[current] = 0
            changed_count += 1

    return (cleaned, changed_count)


def part1(input: str) -> int:
    grid = to_grid(input)
    _, checksum = remove_accessible(grid)
    return checksum


def part2(input: str) -> int:
    grid = to_grid(input)

    checksum = 0
    while True:
        grid, changed_count = remove_accessible(grid)
        if changed_count == 0:
            break
        checksum += changed_count

    return checksum

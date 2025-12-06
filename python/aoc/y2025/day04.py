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


def part1(input: str) -> int:
    grid = to_grid(input)

    checksum = 0
    for (current, value) in grid.items():
        if value == 0:
            continue
        neighbors = get_neighbors(current)
        count = sum(grid.get(p, 0) for p in neighbors)
        if count < 4:
            checksum += 1

    return checksum

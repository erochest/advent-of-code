from aoc.io import read_data_file, read_sample_file
from aoc.y2025.day04 import part1, to_grid


def test_to_grid():
    input = "..@@\n@..@\n@@..\n.@@.\n"
    expected = {
        1+1j: 0, 1+2j: 0, 1+3j: 1, 1+4j: 1,
        2+1j: 1, 2+2j: 0, 2+3j: 0, 2+4j: 1,
        3+1j: 1, 3+2j: 1, 3+3j: 0, 3+4j: 0,
        4+1j: 0, 4+2j: 1, 4+3j: 1, 4+4j: 0,
    }
    actual = to_grid(input)
    assert(actual == expected)


def test_part1_sample_returns_13():
    data = read_sample_file(2025, 4)
    checksum = part1(data)
    assert(checksum == 13)

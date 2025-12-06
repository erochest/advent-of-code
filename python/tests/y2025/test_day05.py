from aoc.io import read_data_file, read_sample_file
from aoc.y2025.day05 import part1, part2


def test_part1_sample_data_returns_3():
    data = read_sample_file(2025, 5)
    checksum = part1(data)
    assert(checksum == 3)

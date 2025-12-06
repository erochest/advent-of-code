

class range_set:
    start: int
    end: int

    def __init__(self, start: int, end: int):
        self.start = start
        self.end = end

    def __contains__(self, i: int) -> bool:
        return self.start <= i <= self.end


type ingredient_info = tuple[list[range_set], list[int]]


def parse_in_stock(line: str) -> int:
    return int(line)


def parse_good_set(line: str) -> range_set:
    start, end = line.split('-')
    return range_set(int(start), int(end))


def parse_input(input: str) -> ingredient_info:
    section = 0
    good = []
    in_stock = []

    for line in input.splitlines():
        if section == 0:
            if len(line) == 0:
                section = 1
            else:
                good.append(parse_good_set(line))
        else:
            in_stock.append(parse_in_stock(line))

    return (good, in_stock)


def part1(input: str) -> int:
    (good_ingredients, in_stock) = parse_input(input)
    count = 0
    for ingredient in in_stock:
        if any(ingredient in rng for rng in good_ingredients):
            count += 1
    return count


def part2(input: str) -> int:
    (good_ingredients, in_stock) = parse_input(input)
    start = min(r.start for r in good_ingredients)
    end = max(r.end for r in good_ingredients)

    count = 0

    for i in range(start, end+1):
        if any(i in rng for rng in good_ingredients):
            count += 1

    return count

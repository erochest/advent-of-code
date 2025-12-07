

class RangeSet:
    start: int
    end: int

    def __init__(self, start: int, end: int):
        self.start = start
        self.end = end

    def __contains__(self, i: int) -> bool:
        return self.start <= i <= self.end

    def superset_of(self, other: 'RangeSet') -> bool:
        """Returns True if every number in `other` is already covered by `self`."""
        return self.start <= other.start and other.end <= self.end

    def extend_with(self, other: 'RangeSet') -> bool:
        """If `other` extends past the end of `self` in either or both directions,
        updates `start` and `end` and returns `True`. If there is no overlap,
        returns `False`."""
        if self.superset_of(other):
            return True

        if other.superset_of(self):
            self.start = other.start
            self.end = other.end
            return True

        if self.start <= other.start < self.end and self.end < other.end:
            self.end = other.end
            return True

        if other.start < self.start and self.start < other.end <= self.end:
            self.start = other.start
            return True

        return False

    @property
    def extent(self):
        return (self.end - self.start) + 1


class RangeSetManager:
    sets: list[RangeSet]

    def __init__(self):
        self.sets = []

    def add_range(self, new_range: RangeSet):
        """Tries to merge the new range with an existing RangeSet.
        Adds it if it cannot."""
        for existing in self.sets:
            if existing.extend_with(new_range):
                return
        self.sets.append(new_range)

    def __contains__(self, i: int) -> bool:
        return any(i in s for s in self.sets)

    @property
    def min(self) -> int:
        return min(s.start for s in self.sets)

    @property
    def max(self) -> int:
        return max(s.end for s in self.sets)


type ingredient_info = tuple[RangeSetManager, list[int]]


def parse_in_stock(line: str) -> int:
    return int(line)


def parse_good_set(line: str) -> RangeSet:
    start, end = line.split('-')
    return RangeSet(int(start), int(end))


def parse_input(input: str) -> ingredient_info:
    section = 0
    good = RangeSetManager()
    in_stock = []

    print('reading good ingredients')
    line_count = 0
    for line in input.splitlines():
        if section == 0:
            if len(line) == 0:
                print(f'read good ingredients: {len(good.sets)} from {line_count} lines')
                print('reading all ingredients')
                section = 1
            else:
                line_count += 1
                good.add_range(parse_good_set(line))
        else:
            in_stock.append(parse_in_stock(line))
    print(f'read {len(in_stock)} ingredients in stock')

    return (good, in_stock)


def part1(input: str) -> int:
    (good_ingredients, in_stock) = parse_input(input)
    count = 0
    for ingredient in in_stock:
        if ingredient in good_ingredients:
            count += 1
    return count


def part2(input: str) -> int:
    (good_ingredients, _) = parse_input(input)
    count = 0

    for range_set in good_ingredients.sets:
        count += range_set.extent

    return count

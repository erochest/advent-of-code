
from advent2022 import day11


def test_run_a():
    input_file = '../sample/2022/day11.txt'
    with open(input_file) as f:
        output = day11.run_a(f)
    assert(10605 == output)


def test_run_b():
    input_file = '../sample/2022/day11.txt'
    with open(input_file) as f:
        output = day11.run_b(f)
    assert(2713310158 == output)


MONKEY1 = '''\
Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0
'''


def test_parse_monkey():
    monkey = day11.Monkey.parse(MONKEY1.splitlines(), 3)
    assert(monkey.n == 1)
    assert(list(monkey.items) == [54, 65, 75, 74])
    assert(monkey.op == (day11.Op.PLUS, 6))
    assert(monkey.test == (day11.Op.DIVISIBLE_BY, 19))
    assert(monkey.action == (2, 0))


def test_monkey_process():
    monkey = day11.Monkey.parse(MONKEY1.splitlines(), 3)
    output = monkey.inspect(54)
    assert(output == (0, 20))
    assert(monkey.inspected == 1)


def test_op_parse():
    tests = [
        ('new = old + 6', (day11.Op.PLUS, 6)),
        ('divisible by 19', (day11.Op.DIVISIBLE_BY, 19)),
        ('new = old * 19', (day11.Op.MULTIPLY, 19)),
        ('new = old * old', (day11.Op.POWER, 2)),
    ]
    for (input_str, expected) in tests:
        assert(day11.Op.parse(input_str) == expected)


def test_op_exec():
    tests = [
        (day11.Op.PLUS, 3, 2, 5),
        (day11.Op.MULTIPLY, 7, 4, 28),
        (day11.Op.SUBTRACT, 7, 4, 3),
        (day11.Op.DIVIDE, 8, 4, 2),
        (day11.Op.DIVISIBLE_BY, 8, 4, 1),
        (day11.Op.DIVISIBLE_BY, 8, 3, 0),
        (day11.Op.POWER, 7, 2, 49),
    ]
    for (op, worry, value, expected) in tests:
        assert(day11.Op.exec(op, worry, value) == expected)
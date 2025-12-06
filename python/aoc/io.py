

def read_sample_file(year: int, day: int, n: int = 0) -> str:
    """return the sample file."""
    filename = f'../sample/{year:04}/day{day:02}-{n}.fixture'
    with open(filename) as f:
        return f.read()


def read_data_file(year: int, day: int) -> str:
    """return the data file."""
    filename = f'../data/{year:04}/day{day:02}.data'
    with open(filename) as f:
        return f.read()



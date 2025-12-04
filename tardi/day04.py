with open('../data/2025/day04.data') as f:
    grid = list(f)

width = len(grid[0].rstrip())
height = len(grid)

access = 0

for x in range(width):
    for y in range(height):
        if grid[y][x] != '@':
            continue
        neighbors = ((a, b) for a in (x-1, x, x+1) for b in (y-1, y, y+1))
        neighbors = ((a, b) for (a, b) in neighbors if not (a == x and b == y))
        neighbors = [(a, b) for (a, b) in neighbors if 0 <= a < width and 0 <= b < height]
        values = [(1 if grid[b][a] == '@' else 0) for (a, b) in neighbors]
        count = sum(values)
        print("{},{}\t{}\t{}\t{}\t{}".format(x, y, neighbors, values, count, 0 < count < 4))
        if 0 < count < 4:
            access += 1

print(f"{width=} {height=}")
print(f"{access=}")

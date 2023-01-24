#!/usr/bin/env python3

fin = open('tmp/day02a.txt')
fout = open('tmp/day02-python.txt', 'w')

lines = list(fin)

for line in lines[::3]:
    parts = line.split()
    char = parts[1][0]
    passwd = parts[2]
    fout.write('%s\n%d\n\n' % (line.strip(), passwd.count(char)))

fout.close()

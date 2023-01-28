#!/usr/bin/env python3

import chevron
import click


NEW_FILES = [
    ('new/day.fth.mustache', 's/%04d/day%02d.fth'),
    ('new/test-day.fth.mustache', 't/%04d/test-day%02d.fth'),
]

APPEND_FILES = [
    ('new/Makefile.mustache', 'Makefile'),
]


@click.command()
@click.option('-y', '--year', default=202, type=int,
              help='The year of Advent of Code. Default is 2020.')
@click.option('-d', '--day', required=True, type=int,
              help='The day to add to the project.')
def main(year, day):
    params = {
        'year': '%04d' % (year,),
        'day': '%02d' % (day,),
        }

    for (src, dest) in NEW_FILES:
        with open(src) as f:
            output = chevron.render(f, params)

        dest = dest % (year, day) if '%' in dest else dest
        with open(dest, 'w') as fout:
            fout.write(output)

    for (src, dest) in APPEND_FILES:
        with open(src) as f:
            output = chevron.render(f, params)
        with open(dest, 'a') as fout:
            fout.write('\n')
            fout.write(output)


if __name__ == '__main__':
    main()

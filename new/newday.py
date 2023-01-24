#!/usr/bin/env python3

import chevron
import click


NEW_FILES = [
    ('new/day.fth.mustache', 's/day%02d.fth'),
    ('new/test-day.fth.mustache', 't/test-day%02d.fth'),
]

APPEND_FILES = [
    ('new/Makefile.mustache', 'Makefile'),
]

TOUCH_FILES = [
    'd/day%02d.txt',
    'f/day%02d.txt',
]


@click.command()
@click.option('-d', '--day', required=True, type=int, help='The day to add to the project.')
def main(day):
    params = {'day': '%02d' % (day,)}

    for (src, dest) in NEW_FILES:
        with open(src) as f:
            output = chevron.render(f, params)

        dest = dest % (day,) if '%' in dest else dest
        with open(dest, 'w') as fout:
            fout.write(output)

    for (src, dest) in APPEND_FILES:
        with open(src) as f:
            output = chevron.render(f, params)
        with open(dest, 'a') as fout:
            fout.write('\n')
            fout.write(output)

    for filename in TOUCH_FILES:
        with open(filename % (day,), 'w') as f:
            f.write('')


if __name__ == '__main__':
    main()

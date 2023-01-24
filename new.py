#!/usr/bin/env python3


import os

import chevron
import click
import yaml


NEW_FILES = [
    ('skel/Main.hs.mustache', 'app/day%d/Main.hs'),
    (None, 'data/day%d.fixture'),
    (None, 'data/day%d.data'),
    ('skel/Day.hs.mustache', 'src/Advent/Day%d.hs'),
    ('skel/Spec.hs.mustache', 'test/day%d/Spec.hs'),
    ]


PACKAGE_FILE = 'package.yaml'


def update_package(day, package_file=PACKAGE_FILE):
    previous = day - 1
    with open(package_file) as fin:
        package = yaml.load(fin, yaml.Loader)

    executable = package['executables']['day%d-exe' % (previous,)].copy()
    executable['source-dirs'] = 'app/day%d' % (day,)
    package['executables']['day%d-exe' % (day,)] = executable
    
    test = package['tests']['day%d-test' % (previous,)].copy()
    test['source-dirs'] = 'test/day%d' % (day,)
    package['tests']['day%d-test' % (day,)] = test

    with open(package_file, 'w') as fout:
        yaml.dump(package, fout)


@click.command()
@click.option('-d', '--day', required=True, type=int, help='The day to add stuff for.')
def main(day):
    params = {'day': '%d' % (day,)}

    for (src, dest) in NEW_FILES:
        dest = dest % (day,)

        if src is None:
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            f = open(dest, 'w')
            f.close()

        else:
            with open(src) as fin:
                output = chevron.render(fin, params)
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            with open(dest, 'w') as fout:
                fout.write(output)

    update_package(day)


if __name__ == '__main__':
    main()
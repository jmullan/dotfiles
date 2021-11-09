#!/usr/bin/env python
import os
import re

from optparse import OptionParser


def main():
    """Sort a properties file"""
    changed = False
    parser = OptionParser()
    parser.add_option('-v', '--verbose', dest='verbose',
                      action='store_true', default=False,
                      help='verbose is more verbose')
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get('verbose')

    values_by_file = {}
    defaults = {}
    for filename in args:
        values_by_file[filename] = {}
        contents = ''
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        for x in contents.split('\n'):
            x = x.strip()
            if not x.startswith("#") and len(x) > 0 and '=' in x:
                key, value = x.split('=', 1)
                values_by_file[filename][key] = value
                defaults[key] = value
    changed_files = set()
    for filename, properties in values_by_file.items():
        for key, value in defaults.items():
            if key not in properties:
                properties[key] = value
                changed_files.add(filename)

    for filename in changed_files:
        new_contents = [
            '%s=%s' % (key, value)
            for key, value in values_by_file[filename].items()
        ]
        new_contents = sorted(new_contents)

        new_contents = '\n'.join(new_contents)
        if len(new_contents) > 0:
            new_contents = new_contents + '\n'
        if verbose:
            print('updated file %s' % filename)
        with open(filename, 'w') as f:
            f.write(new_contents)


if __name__ == "__main__":
    main()

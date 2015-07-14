#!/usr/bin/env python
import os
from optparse import OptionParser

REGEX = r'([^ (]+).has_key\(([^)]+)\)'


def main():
    """Do it."""
    changed = False
    parser = OptionParser()
    parser.add_option('-v', '--verbose', dest='verbose',
                      action='store_true', default=False,
                      help='verbose is more verbose')
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get('verbose')

    for filename in args:
        contents = ''
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        while contents[-2:] == "\n\n":
            contents = contents[:-1]
            changed = True
        if changed:
            if verbose:
                print 'updated file %s' % filename
            with open(filename, 'w') as f:
                f.write(contents)


if __name__ == "__main__":
    main()

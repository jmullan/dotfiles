#!/usr/bin/env python
import os
import sys
from optparse import OptionParser


def main():
    """Strip extra newlines from end of files, add one if there is none."""
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
        while len(contents) and contents[0] == "\n":
            contents = contents[1:]
            changed = True
        while contents[-2:] == "\n\n":
            contents = contents[:-1]
            changed = True
        if contents and contents[-1] != "\n":
            contents = "%s\n" % contents
            changed = True
        if changed:
            if verbose:
                sys.stdout.write('updated file %s' % filename)
            with open(filename, 'w') as f:
                f.write(contents)


if __name__ == "__main__":
    main()

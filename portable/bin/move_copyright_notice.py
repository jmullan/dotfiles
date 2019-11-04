#!/usr/bin/env python
import os
import re
import sys
from optparse import OptionParser

DESIRED = '// Copyright {date_stuff} {who}'
STAR_COMMENT_COPYRIGHT_REGEX = r'^\s*\*\s*(Copyright.*)$'


def update_contents(contents, verbose):
    copyright_dict = {
        "date_stuff": '2019',
        "who": 'Pandora Media Inc.'
    }
    first_line, new_line, remainder = contents.partition("\n")
    if first_line.startswith('//') and ' copyright ' in first_line.lower():
        if verbose:
            print("No change, found: %s", first_line)
        return contents
    matches = re.search(STAR_COMMENT_COPYRIGHT_REGEX, contents)
    print("matches", matches)
    print("contents", contents)
    if matches:
        if verbose:
            print("Moving copyright to first line: %s" % matches.group(0))
        copyright_line = '// ' + matches.group(1).trim()
    else:
        copyright_line = DESIRED.format(**copyright_dict)
        if verbose:
            print("Making a brand new copyright line: %s" % copyright_line)
        exit(1)
    return copyright_line + new_line + first_line + new_line + remainder


def process_file(filename, verbose):
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            original_contents = f.read(filesize)
            contents = original_contents
        contents = update_contents(contents, verbose)
        changed = contents != original_contents
        if changed:
            if verbose:
                sys.stdout.write('updated file %s' % filename)
            with open(filename, 'w') as f:
                f.write(contents)


def main():
    """Strip $Id$ and stuff."""
    changed = False
    parser = OptionParser()
    parser.add_option('-v', '--verbose', dest='verbose',
                      action='store_true', default=False,
                      help='verbose is more verbose')
    parser.add_option('-d', '--diagnostic', dest='diagnostic',
                      action='store_true', default=False,
                      help='run some assertions first')
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get('verbose')
    if options.get('diagnostic'):
        re.search(STAR_COMMENT_COPYRIGHT_REGEX, ' * Copyright 2007 Pandora Media, Inc.')
        comment = """\n\n/**\n * Copyright 2007 Pandora Media, Inc.\n */\npublic class VendorSku"""
        re.search(STAR_COMMENT_COPYRIGHT_REGEX, comment)
    for filename in args:
        process_file(filename, verbose)


if __name__ == "__main__":
    main()

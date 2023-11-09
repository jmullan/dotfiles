#!/usr/bin/env python-venv dotfiles
import os
import re
import sys
from optparse import OptionParser


def blank(line):
    if re.match(r"^ +$", line):
        return ""
    else:
        return line


def main():
    """Remove trailing whitespace"""
    parser = OptionParser()
    parser.add_option(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_option(
        "--only-blank-lines",
        dest="only_blank_lines",
        action="store_true",
        default=False,
        help="Only trim blank lines",
    )
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get("verbose")
    only_blank_lines = options.get("only_blank_lines")

    for filename in args:
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        if only_blank_lines:
            new_contents = "\n".join(blank(x) for x in contents.split("\n"))
        else:
            new_contents = "\n".join(x.rstrip() for x in contents.split("\n"))
        changed = new_contents != contents
        if changed:
            if verbose:
                sys.stdout.write("updated file %s\n" % filename)
            with open(filename, "w") as f:
                f.write(new_contents)


if __name__ == "__main__":
    main()

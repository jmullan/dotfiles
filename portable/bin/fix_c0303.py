#!/usr/bin/env python-venv
import os
import re
import sys
from argparse import ArgumentParser


def blank(line):
    if re.match(r"^ +$", line):
        return ""
    else:
        return line


def main():
    """Remove trailing whitespace"""
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument(
        "--only-blank-lines",
        dest="only_blank_lines",
        action="store_true",
        default=False,
        help="Only trim blank lines",
    )
    parser.add_argument("filenames", nargs="+")
    args = parser.parse_args()
    verbose = args.verbose
    only_blank_lines = args.only_blank_lines

    for filename in args.filenames:
        file_size = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(file_size)
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

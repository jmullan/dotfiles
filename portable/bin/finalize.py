#!/usr/bin/env python-venv
import os
import re
import sys
from argparse import ArgumentParser


def main():
    """Strip $Id$ and stuff."""
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument("filenames", nargs="+")

    args = parser.parse_args()
    verbose = args.verbose

    for filename in args.filenames:
        print(filename)
        file_size = os.path.getsize(filename)
        with open(filename) as f:
            original_contents = f.read(file_size)
            contents = original_contents

        pattern = r"private final ([A-Za-z]+) ([a-zA-Z_]+) ="
        replacement = r"private static final \1 \2 ="

        contents = re.sub(pattern, replacement, contents)

        changed = contents != original_contents
        if changed:
            if verbose:
                sys.stdout.write("updated file %s" % filename)
            with open(filename, "w") as f:
                f.write(contents)


if __name__ == "__main__":
    main()

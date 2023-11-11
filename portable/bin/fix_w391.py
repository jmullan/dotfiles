#!/usr/bin/env python-venv
import os
import sys
from argparse import ArgumentParser


def main():
    """Strip extra newlines from end of files, add one if there is none."""
    changed = False
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument('filenames', nargs='+')

    args = parser.parse_args()
    verbose = args.verbose

    for filename in args.filenames:
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
                sys.stdout.write("updated file %s" % filename)
            with open(filename, "w") as f:
                f.write(contents)


if __name__ == "__main__":
    main()

#!/usr/bin/env python-venv
import os
import re
import sys
from argparse import ArgumentParser


def main():
    """Snug up final curly braces"""
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
    args = parser.parse_args()
    options = options.__dict__
    verbose = args.verbose

    for filename in args:
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            original_contents = f.read(filesize)
            contents = original_contents

        replace_patterns = [(r"\n\n+package", r"\npackage"), (r"\n\n\n\n+", r"\n\n\n")]
        for pattern, replacement in replace_patterns:
            contents = re.sub(pattern, replacement, contents)

        changed = contents != original_contents
        if changed:
            if verbose:
                sys.stdout.write("updated file %s" % filename)
            with open(filename, "w") as f:
                f.write(contents)


if __name__ == "__main__":
    main()

#!/usr/bin/env python-venv
import os

from optparse import OptionParser


def k_v(line: str):
    k, v = line.split("=", 1)
    return k, v


def main():
    """Sort a properties file"""
    parser = OptionParser()
    parser.add_option(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get("verbose")

    for filename in args:
        contents = ""
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        new_contents = [
            x.strip()
            for x in contents.split("\n")
            if not x.startswith("#") and "=" in x
        ]

        new_contents = dict([k_v(x) for x in reversed(new_contents) if len(x) > 0])
        new_contents = [f"{k}={v}" for k, v in new_contents.items()]

        new_contents = "\n".join(sorted(new_contents))
        if len(new_contents) > 0:
            new_contents = new_contents + "\n"
        changed = new_contents != contents
        if changed:
            if verbose:
                print("updated file %s" % filename)
            with open(filename, "w") as f:
                f.write(new_contents)


if __name__ == "__main__":
    main()

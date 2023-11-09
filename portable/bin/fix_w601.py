#!/usr/bin/env python-venv dotfiles
import os
import re
from optparse import OptionParser

REGEX = r"([^ (]+).has_key\(([^)]+)\)"


def main():
    """Try to turn has_key into ` in `."""
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
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        changed = False
        contents_was = None
        while contents != contents_was:
            contents_was = contents
            match = re.search(REGEX, contents)
            if match:
                find = match.group(0)
                if verbose:
                    print("Found match %s in %s" % (find, filename))
                replace = "%s in %s" % (match.group(2), match.group(1))
                changed = True
                contents = contents.replace(find, replace)
            else:
                if verbose:
                    print("no match", REGEX)

        if changed:
            if verbose:
                print("updated file %s" % filename)
            with open(filename, "w") as f:
                f.write(contents)


if __name__ == "__main__":
    main()

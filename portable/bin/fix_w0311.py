#!/usr/bin/env python
import os
import re

from optparse import OptionParser


def main():
    """Reindent a python file."""
    changed = False
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
        "-w", "--width", dest="width", default=4, type=int, help="Indent by this much"
    )
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get("verbose")
    width = options.get("width")

    for filename in args:
        contents = ""
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        new_contents = []
        indentation_level = []
        prior_indentation = ""
        lines = contents.split("\n")
        for line_number, line in enumerate(lines):
            if len(line):
                matches = re.match("^( *)(.*)$", line)
                indentation = matches.group(1)
                remainder = matches.group(2)
                this_indentation = indentation
                if len(indentation) > len(prior_indentation):
                    print(">" * width, line)
                    diff = len(indentation) - len(prior_indentation)
                    indentation_level.append(" " * diff)
                elif len(indentation) < len(prior_indentation):
                    print("<" * width, line)
                    while len("".join(indentation_level)) > len(indentation):
                        indentation_level.pop()
                else:
                    print(" " * width, line)
                indentation = len(indentation_level) * width * " "
                prior_indentation = this_indentation
                line = "%s%s" % (indentation, remainder)
            new_contents.append(line)
        new_contents = "\n".join(new_contents)
        changed = new_contents != contents
        if changed:
            if verbose:
                print("updated file %s" % filename)
            with open(filename, "w") as f:
                f.write(new_contents)


if __name__ == "__main__":
    main()

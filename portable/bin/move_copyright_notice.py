#!/usr/bin/env python-venv dotfiles
import os
import re
from optparse import OptionParser

DESIRED = "// Copyright {date_stuff} {who}"
STAR_COMMENT_COPYRIGHT_REGEX = r"(?:^|\n)\s*\*\s*(Copyright[^\n]*)(?:$|\n)"
SLASH_COMMENT_COPYRIGHT_REGEX = r"(?:^|\n)\s*//\s*(Copyright[^\n]*)(?:$|\n)"


def update_contents(contents, verbose):
    copyright_dict = {"date_stuff": "2020", "who": "Pandora Media Inc."}
    first_line, new_line, remainder = contents.partition("\n")
    if first_line.startswith("//") and "copyright" in first_line.lower():
        if verbose:
            print("No change, found: %s", first_line)
        return contents
    updated = False
    matches = re.search(STAR_COMMENT_COPYRIGHT_REGEX, contents)
    if matches:
        copyright = matches.group(0)
        if verbose:
            print("Moving star copyright to first line: %r" % copyright)
        copyright_line = "// " + matches.group(1).strip()
        contents = contents.replace(copyright.strip("\\n"), "\n")
        updated = True

    matches = re.search(SLASH_COMMENT_COPYRIGHT_REGEX, contents)
    if matches:
        copyright = "%s" % matches.group(0)
        if verbose:
            print("Moving slash copyright to first line: %r" % copyright)
        copyright_line = "// " + matches.group(1).strip()
        contents = contents.replace(copyright.strip("\n"), "\n")
        updated = True

    if not updated:
        copyright_line = DESIRED.format(**copyright_dict)
        if verbose:
            print("Making a brand new copyright line: %r" % copyright_line)
        exit(1)
    return copyright_line + new_line + contents


def process_file(filename, verbose):
    filesize = os.path.getsize(filename)
    with open(filename) as f:
        original_contents = f.read(filesize)
        contents = original_contents
    contents = update_contents(contents, verbose)
    changed = contents != original_contents
    if changed:
        if verbose:
            print("updated file %s" % filename)
        with open(filename, "w") as f:
            f.write(contents)


def main():
    """Strip $Id$ and stuff."""
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
        "-d",
        "--diagnostic",
        dest="diagnostic",
        action="store_true",
        default=False,
        help="run some assertions first",
    )
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get("verbose")
    if options.get("diagnostic"):
        assert re.search(
            STAR_COMMENT_COPYRIGHT_REGEX, " * Copyright 2011, Pandora Media, Inc."
        )

        assert re.search(
            STAR_COMMENT_COPYRIGHT_REGEX, " * Copyright 2007 Pandora Media, Inc."
        )
        comment = """\n\n/**\n * Copyright 2007 Pandora Media, Inc.\n */\npublic class VendorSku"""
        assert re.search(STAR_COMMENT_COPYRIGHT_REGEX, comment)

        comment = """\n// Copyright (c) 2007 SavageBeast Technologies Inc.\n"""
        assert re.search(SLASH_COMMENT_COPYRIGHT_REGEX, comment)

    for filename in args:
        process_file(filename, verbose)


if __name__ == "__main__":
    main()

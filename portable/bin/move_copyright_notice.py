#!/usr/bin/env python-venv
import datetime
import os
import re
from argparse import ArgumentParser
from jmullanpy import cmd

DESIRED = "// Copyright {date_stuff} {who}"
STAR_COMMENT_COPYRIGHT_REGEX = r"(?:^|\n)\s*\*\s*(Copyright[^\n]*)(?:$|\n)"
SLASH_COMMENT_COPYRIGHT_REGEX = r"(?:^|\n)\s*//\s*(Copyright[^\n]*)(?:$|\n)"
WHO = os.environ.get("DEFAULT_COPYRIGHT", "Jesse Mullan")


def update_contents(contents: str, verbose) -> str:
    copyright_line = ""
    copyright_dict = {
        "date_stuff": datetime.datetime.now().strftime("%Y"),
        "who": WHO,
    }
    first_line, new_line, remainder = contents.partition("\n")
    if first_line.startswith("//") and "copyright" in first_line.lower():
        if verbose:
            print("No change, found: %s", first_line)
        return contents
    updated = False
    matches = re.search(STAR_COMMENT_COPYRIGHT_REGEX, contents)
    if matches:
        copyright_text = matches.group(0)
        if verbose:
            print("Moving star copyright to first line: %r" % copyright_text)
        copyright_line = "// " + matches.group(1).strip()
        contents = contents.replace(copyright_text.strip("\\n"), "\n")

    matches = re.search(SLASH_COMMENT_COPYRIGHT_REGEX, contents)
    if matches:
        copyright_text = "%s" % matches.group(0)
        if verbose:
            print("Moving slash copyright to first line: %r" % copyright_text)
        copyright_line = "// " + matches.group(1).strip()
        contents = contents.replace(copyright_text.strip("\n"), "\n")

    if not updated:
        copyright_line = DESIRED.format(**copyright_dict)
        if verbose:
            print("Making a brand new copyright line: %r" % copyright_line)
    return copyright_line + new_line + contents


def main():
    """Strip $Id$ and stuff."""
    cmd.stop_on_broken_pipe_error()
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
        "-d",
        "--diagnostic",
        dest="diagnostic",
        action="store_true",
        default=False,
        help="run some assertions first",
    )
    cmd.add_filenames_arguments(parser)
    args = parser.parse_args()
    if args.diagnostic:
        assert re.search(
            STAR_COMMENT_COPYRIGHT_REGEX, " * Copyright 2011, Some Company"
        )

        assert re.search(
            STAR_COMMENT_COPYRIGHT_REGEX, " * Copyright 2007 Some other company"
        )
        comment = """\n\n/**\n * Copyright 2007 Company Company, Inc.\n */\npublic class VendorSku"""
        assert re.search(STAR_COMMENT_COPYRIGHT_REGEX, comment)

        comment = """\n// Copyright (c) 2007 Any Technologies Inc.\n"""
        assert re.search(SLASH_COMMENT_COPYRIGHT_REGEX, comment)

    def process(a_filename):
        return update_contents(a_filename, args.verbose)

    for filename in cmd.get_filenames(args):
        cmd.update_in_place(filename, process)


if __name__ == "__main__":
    main()

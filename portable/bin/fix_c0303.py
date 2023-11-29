#!/usr/bin/env python-venv
import re
from argparse import ArgumentParser

from jmullanpy.command_helpers import (
    stop_on_broken_pipe_error,
    get_filenames,
    add_filenames_arguments,
    update_in_place,
)


def blank(line: str | None) -> str | None:
    if line is None:
        return None
    if re.match(r"^ +$", line):
        return ""
    else:
        return line


def trim_blank_lines(contents: str) -> str:
    return "\n".join(blank(x) for x in contents.split("\n"))


def remove_trailing_whitespace(contents: str) -> str:
    return "\n".join(x.rstrip() for x in contents.split("\n"))


def main():
    """Remove trailing whitespace"""
    stop_on_broken_pipe_error()
    parser = ArgumentParser(description="Remove Trailing Whitespace")
    parser.add_argument(
        "--only-blank-lines",
        dest="only_blank_lines",
        action="store_true",
        default=False,
        help="Only trim blank lines",
    )
    add_filenames_arguments(parser)
    args = parser.parse_args()
    only_blank_lines = args.only_blank_lines

    for filename in get_filenames(args):
        if only_blank_lines:
            update_in_place(filename, trim_blank_lines)
        else:
            update_in_place(filename, remove_trailing_whitespace)


if __name__ == "__main__":
    main()

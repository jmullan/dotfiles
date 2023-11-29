#!/usr/bin/env python-venv
import re
from argparse import ArgumentParser, Namespace

from jmullanpy.command_helpers import (
    stop_on_broken_pipe_error,
    get_filenames,
    add_filenames_arguments,
    update_in_place,
)


def munge(contents: str, args: Namespace) -> str:
    lines = contents.split("\n")
    stripped_lines = [line.rstrip() for line in lines]
    contents = "\n".join(stripped_lines)
    if not contents.endswith("\n"):
        contents = contents + "\n"

    if args.tabs:
        use = "tabs"
    elif args.spaces:
        use = "spaces"
    else:
        tabs = 0
        spaces = 0
        use = "spaces"
        for line in stripped_lines:
            if line.startswith("\t"):
                tabs += 1
            if line.startswith(" "):
                spaces += 1
        if tabs > spaces:
            use = "tabs"

    lines = contents.split("\n")
    new_lines = []
    for line in lines:
        matches = re.match(r"^([ \t]+)(.*)", line)

        if matches:
            leading_whitespace = matches.group(1)
            rest_of_line = matches.group(2)
            if use == "spaces" and "\t" in leading_whitespace:
                leading_whitespace = leading_whitespace.replace(
                    "\t", " " * args.tab_width
                )
            elif use == "tabs" and " " in leading_whitespace:
                leading_whitespace = leading_whitespace.replace(
                    " " * args.tab_width, "\t"
                )
            line = f"{leading_whitespace}{rest_of_line}"
        new_lines.append(line)

    return "\n".join(lines)


def main():
    """Strip $Id$ and stuff."""
    stop_on_broken_pipe_error()
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
        "-n",
        "--noop",
        dest="noop",
        action="store_true",
        default=False,
        help="Dry run only",
    )
    parser.add_argument(
        "-s",
        "--spaces",
        dest="spaces",
        action="store_true",
        default=False,
        help="Ignore tabs, embrace spaces",
    )
    parser.add_argument(
        "-t",
        "--tabs",
        dest="tabs",
        action="store_true",
        default=False,
        help="Ignore spaces, embrace tabs",
    )
    parser.add_argument(
        "-t",
        "--tab-width",
        dest="tab_width",
        type=int,
        default=4,
        help="How many spaces equal a tab",
    )
    add_filenames_arguments(parser)
    args = parser.parse_args()

    def munge_closure(contents: str) -> str:
        return munge(contents, args)

    for filename in get_filenames(args):
        update_in_place(filename, munge_closure)


if __name__ == "__main__":
    main()

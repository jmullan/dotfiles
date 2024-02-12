#!/usr/bin/env python-venv
"""Fix mixed tabs and spaces"""
import re
from argparse import Namespace

from jmullan_cmd import cmd


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


class Main(cmd.InPlaceFileProcessor):
    def __init__(self):
        super().__init__()
        self.parser.add_argument(
            "-n",
            "--noop",
            dest="noop",
            action="store_true",
            default=False,
            help="Dry run only",
        )
        self.parser.add_argument(
            "-s",
            "--spaces",
            dest="spaces",
            action="store_true",
            default=False,
            help="Ignore tabs, embrace spaces",
        )
        self.parser.add_argument(
            "-t",
            "--tabs",
            dest="tabs",
            action="store_true",
            default=False,
            help="Ignore spaces, embrace tabs",
        )
        self.parser.add_argument(
            "--tab-width",
            dest="tab_width",
            type=int,
            default=4,
            help="How many spaces equal a tab",
        )

    def process_contents(self, contents: str) -> str:
        return munge(contents, self.args)


if __name__ == "__main__":
    Main().main()

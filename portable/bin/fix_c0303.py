#!/usr/bin/env python-venv
"""Remove trailing whitespace"""
import re

from jmullanpy import cmd


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


class Main(cmd.InPlaceFileProcessor):
    def __init__(self):
        super().__init__()
        self.parser.add_argument(
            "--only-blank-lines",
            dest="only_blank_lines",
            action="store_true",
            default=False,
            help="Only trim blank lines",
        )

    def process_contents(self, contents: str) -> str:
        if self.args.only_blank_lines:
            return trim_blank_lines(contents)
        else:
            return remove_trailing_whitespace(contents)


if __name__ == "__main__":
    Main().main()

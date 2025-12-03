#!/usr/bin/env -S python-venv --virtualenv dotfiles dotfiles
"""Strip extra newlines from end of files, add one if there is none."""

import logging
from jmullan.cmd import cmd
from jmullan.logging.easy_logging import easy_initialize_logging

logger = logging.getLogger(__name__)


class Main(cmd.InPlaceFileProcessor):

    def setup(self):
        super().setup()
        if self.args.verbose:
            easy_initialize_logging("DEBUG", stream=sys.stderr)
        else:
            easy_initialize_logging("INFO", stream=sys.stderr)

    def process_contents(self, contents: str) -> str:
        return process_contents(contents)


def process_contents(contents: str) -> str:
    if contents is None or not len(contents):
        return ""
    index = len(contents) - 1
    print(index)
    while index >= 0 and contents[index] in " \t\n\r":
        index -= 1
        print(index)
    if index == len(contents):
        return contents
    return contents[: index + 1] + "\n"


def test_empty_file():
    assert process_contents(None) == ""
    assert process_contents("") == ""


def test_happy_path():
    assert process_contents("f") == "f\n"
    assert process_contents("foo") == "foo\n"
    assert process_contents("foo\n") == "foo\n"
    assert process_contents("foo\n\n\n") == "foo\n"
    assert process_contents("\nfoo\n\n") == "\nfoo\n"


if __name__ == "__main__":
    Main().main()

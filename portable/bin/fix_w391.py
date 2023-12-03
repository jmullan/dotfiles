#!/usr/bin/env python-venv
from jmullanpy import cmd


class Main(cmd.InPlaceFileProcessor):
    """Strip extra newlines from end of files, add one if there is none."""

    def process_contents(self, contents: str) -> str:
        while len(contents) and contents[0] == "\n":
            contents = contents[1:]
        while contents[-2:] == "\n\n":
            contents = contents[:-1]
        if contents and contents[-1] != "\n":
            contents = "%s\n" % contents
        return contents


if __name__ == "__main__":
    Main().main()

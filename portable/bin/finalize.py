#!/usr/bin/env -S python-venv --virtualenv dotfiles
from jmullan_cmd import cmd
import re


class Main(cmd.InPlaceFileProcessor):
    def process_contents(self, contents: str) -> str:

        pattern = r"private final ([A-Za-z]+) ([a-zA-Z_]+) ="
        replacement = r"private static final \1 \2 ="

        return re.sub(pattern, replacement, contents)


if __name__ == "__main__":
    Main().main()

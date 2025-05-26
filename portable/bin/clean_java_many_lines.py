#!/usr/bin/env -S python-venv --virtualenv dotfiles
import re

from jmullan.cmd import cmd


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
            "-t",
            "--tab-width",
            dest="tab_width",
            type=int,
            default=4,
            help="How many spaces equal a tab",
        )

    def process_contents(self, contents: str) -> str:
        replace_patterns = [(r"\n\n+package", r"\npackage"), (r"\n\n\n+", r"\n\n")]
        for pattern, replacement in replace_patterns:
            contents = re.sub(pattern, replacement, contents)
        return contents


if __name__ == "__main__":
    Main().main()

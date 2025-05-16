#!/usr/bin/env -S python-venv --virtualenv dotfiles dotfiles
"""Fix bad indentation"""

import re

from jmullan_cmd import cmd


class Main(cmd.InPlaceFileProcessor):
    def __init__(self):
        super().__init__()
        self.parser.add_argument(
            "-w",
            "--width",
            dest="width",
            default=4,
            type=int,
            help="Indent by this much",
        )

    def process_contents(self, contents: str) -> str:
        width = self.args.width

        new_contents = []
        indentation_level = []
        prior_indentation = ""
        lines = contents.split("\n")
        for line_number, line in enumerate(lines):
            if len(line):
                matches = re.match("^( *)(.*)$", line)
                indentation = matches.group(1)
                remainder = matches.group(2)
                this_indentation = indentation
                if len(indentation) > len(prior_indentation):
                    if self.args.verbose:
                        print(">" * width, line)
                    diff = len(indentation) - len(prior_indentation)
                    indentation_level.append(" " * diff)
                elif len(indentation) < len(prior_indentation):
                    if self.args.verbose:
                        print("<" * width, line)
                    while len("".join(indentation_level)) > len(indentation):
                        indentation_level.pop()
                else:
                    if self.args.verbose:
                        print(" " * width, line)
                indentation = len(indentation_level) * width * " "
                prior_indentation = this_indentation
                line = "%s%s" % (indentation, remainder)
            new_contents.append(line)
        return "\n".join(new_contents)


if __name__ == "__main__":
    Main().main()

#!/usr/bin/env -S python-venv --virtualenv dotfiles
import re

from jmullan.cmd import cmd

REGEX = r"([^ (]+).has_key\(([^)]+)\)"


class Main(cmd.InPlaceFileProcessor):
    """Try to turn has_key into ` in `."""

    def process_contents(self, contents: str) -> str:
        contents_was = None
        while contents != contents_was:
            contents_was = contents
            match = re.search(REGEX, contents)
            if match:
                find = match.group(0)
                replace = "%s in %s" % (match.group(2), match.group(1))
                contents = contents.replace(find, replace)
            else:
                if self.args.verbose:
                    print("no match", REGEX)
        return contents


if __name__ == "__main__":
    Main().main()

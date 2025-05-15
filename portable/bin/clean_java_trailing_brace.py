#!/usr/bin/env -S python-venv --virtualenv dotfiles
import re

from jmullan_cmd import cmd


class Main(cmd.InPlaceFileProcessor):
    """Remove trailing whitespace"""

    def process_contents(self, contents: str) -> str:
        replace_patterns = [(r"\n+}[\n\s]*\Z", r"\n}\n"), (r"}[\n\s]+}\n\Z", r"}\n}\n")]
        for pattern, replacement in replace_patterns:
            contents = re.sub(pattern, replacement, contents)
        if len(contents) > 1 and not contents.endswith("\n"):
            contents = contents + "\n"
        return contents


if __name__ == "__main__":
    Main().main()

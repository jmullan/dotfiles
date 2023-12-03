#!/usr/bin/env python-venv

from jmullanpy import cmd


def k_v(line: str):
    k, v = line.split("=", 1)
    return k, v


class Main(cmd.InPlaceFileProcessor):
    """Remove trailing whitespace"""

    def process_contents(self, contents: str) -> str:
        new_contents = [
            x.strip()
            for x in contents.split("\n")
            if not x.startswith("#") and "=" in x
        ]

        new_contents = dict([k_v(x) for x in reversed(new_contents) if len(x) > 0])
        new_contents = [f"{k}={v}" for k, v in new_contents.items()]

        new_contents = "\n".join(sorted(new_contents))
        if len(new_contents) > 0:
            new_contents = new_contents + "\n"
        return new_contents


if __name__ == "__main__":
    Main().main()

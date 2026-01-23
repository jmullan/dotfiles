#!/usr/bin/env -S python-venv --virtualenv dotfiles
import datetime
import os
import re
import sys
import subprocess

from jmullan.cmd import cmd
from jmullan.logging.easy_logging import easy_initialize_logging

import logging

logger = logging.getLogger(__name__)

def run(command: list[str]) -> list[str]:
    logger.debug(f"{' '.join(command)}")
    with subprocess.Popen(
        command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding="UTF8"
    ) as proc:
        std_out = [
            x for x in [line.strip() for line in proc.stdout.readlines()] if len(x)
        ]
        std_err = [
            x for x in [line.strip() for line in proc.stderr.readlines()] if len(x)
        ]
        if std_err:
            logger.debug(f"{std_err}")
        return std_out


def first_or_none(maybes: list[str] | None) -> str | None:
    if not maybes:
        return None
    maybes = [x.strip() for x in maybes if x is not None]
    likelies = [y for y in maybes if len(y) > 0]
    if likelies:
        return likelies[0]
    return None


DESIRED = "// Copyright {date_stuff} {who}"
STAR_COMMENT_COPYRIGHT_REGEX = r"(?:^|\n)\s*\*\s*(Copyright[^\n]*)(?:$|\n)"
SLASH_COMMENT_COPYRIGHT_REGEX = r"(?:^|\n)\s*//\s*(Copyright[^\n]*)(?:$|\n)"
DEFAULT_WHO = os.environ.get("DEFAULT_COPYRIGHT") or first_or_none(run(["gecos", "--fullname"]))



class Main(cmd.InPlaceFileProcessor):
    def __init__(self):
        super().__init__()
        self.parser.add_argument(
            "--add-if-missing",
            dest="add_if_missing",
            action="store_true",
            default=False,
            help="Add a copyright if there is none.",
        )
        if DEFAULT_WHO:
            who_help = f"Who to blame in the copyright. Default: {DEFAULT_WHO}"
        else:
            who_help = f"Who to blame in the copyright."
        self.parser.add_argument(
            "--who",
            dest="who",
            default=DEFAULT_WHO,
            help=who_help
        )


    def setup(self):
        super().setup()
        if self.args.verbose:
            easy_initialize_logging("DEBUG", stream=sys.stderr)
        else:
            easy_initialize_logging("INFO", stream=sys.stderr)

    def process_contents(self, contents: str) -> str:
        assert re.search(
            STAR_COMMENT_COPYRIGHT_REGEX, " * Copyright 2011, Some Company"
        )

        assert re.search(
            STAR_COMMENT_COPYRIGHT_REGEX, " * Copyright 2007 Some other company"
        )
        comment = """\n\n/**\n * Copyright 2007 Company Company, Inc.\n */\npublic class VendorSku"""
        assert re.search(STAR_COMMENT_COPYRIGHT_REGEX, comment)

        comment = """\n// Copyright (c) 2007 Any Technologies Inc.\n"""
        assert re.search(SLASH_COMMENT_COPYRIGHT_REGEX, comment)

        this_year = datetime.datetime.now().strftime("%Y")
        copyright_line = ""
        copyright_dict = {
            "date_stuff": this_year,
            "who": self.args.who,
        }
        desired_copyright_line = DESIRED.format(**copyright_dict)
        first_line, new_line, remainder = contents.partition("\n")
        if first_line.startswith("//") and "copyright" in first_line.lower():
            logger.debug("No change, found: %s", first_line)
            copyright_line = first_line
            contents = remainder
        else:
            matches = re.search(STAR_COMMENT_COPYRIGHT_REGEX, contents)
            if matches:
                copyright_text = matches.group(0)
                logger.debug("Moving star copyright to first line: %r", copyright_text)
                copyright_line = "// " + matches.group(1).strip()
                contents = contents.replace(copyright_text.strip("\\n"), "\n")

            matches = re.search(SLASH_COMMENT_COPYRIGHT_REGEX, contents)
            if matches:
                copyright_text = "%s" % matches.group(0)
                logger.debug("Moving slash copyright to first line: %r", copyright_text)
                copyright_line = "// " + matches.group(1).strip()
                contents = contents.replace(copyright_text.strip("\n"), "\n")

        if not copyright_line and self.args.add_if_missing:
            logger.debug("Making a brand new copyright line: %r", copyright_line)
            copyright_line = desired_copyright_line

        if copyright_line:
            if this_year not in copyright_line:
                copyright_line = desired_copyright_line
            return copyright_line + new_line + contents
        else:
            return contents


def main():
    Main().main()


if __name__ == "__main__":
    main()

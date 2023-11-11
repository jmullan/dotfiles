#!/usr/bin/env python-venv
import re
import sys

from argparse import ArgumentParser

# remoting/__init__.py                 6      0   100%
# remoting/jsonrpc.py                 59     33    44%   7-10, 14, 30, 38, 41, 46, 51, 54-59, 67, 74-111
# remoting/main.py                    19     19     0%   2-56
# remoting/metadata.py                12      0   100%
# remoting/tests/test_example.py      10      0   100%


PARTS = (
    r"^(?P<filename>.*.py)"
    r"\s+(?P<lines>[0-9]+)"
    r"\s+(?P<uncovered>[0-9]+)"
    r"\s+(?P<percent>[0-9]+)%"
    r"\s*(?P<ranges>.*)"
)
RANGE = r"(?P<start>[0-9]+)-(?P<end>[0-9]+)"


def dump(filename, ranges):
    message = "{0}:{1}:1:Code not covered\n"
    for line_range in ranges:
        match = re.match(RANGE, line_range)
        if match:
            start = int(match.group("start"))
            end = int(match.group("end"))
        else:
            start = int(line_range)
            end = start
        for line_number in range(start, end + 1):
            sys.stdout.write(message.format(filename, line_number))


def main():
    """Reindent a python file."""
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument('filenames', nargs='+')
    args = parser.parse_args()
    lines = [line.strip() for line in sys.stdin]

    if not lines:
        return
    lines = lines[2:-2]

    results = {}

    for line in lines:
        match = re.match(PARTS, line)
        if not match:
            continue
        filename = match.group("filename")
        lines = int(match.group("lines"))
        uncovered = int(match.group("uncovered"))
        if not uncovered:
            continue
        # percent = int(match.group('percent'))
        ranges = match.group("ranges").split(", ")
        results[filename] = ranges

    for filename in args.filenames:
        flymake_name = filename.replace(".py", "_flymake.py")
        if filename in results:
            dump(filename, results[filename])
        elif flymake_name in results:
            dump(flymake_name, results[filename])


if __name__ == "__main__":
    main()

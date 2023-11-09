#!/usr/bin/env python-venv dotfiles
"""Back up a file or files."""
import argparse
import logging
import os
import re
import subprocess

FORMAT = "%(levelname)s %(lineno)d %(message)s"
logging.basicConfig(format=FORMAT)


def main():
    """Do it."""
    parser = argparse.ArgumentParser()
    parser.add_argument("url_files", nargs="+")
    args = parser.parse_args()
    for url_file in args.url_files:
        lines = [line.strip() for line in open(url_file)]
        for line in lines:
            matches = re.match(r"URL=(.*)", line)
            if matches is not None:
                url = matches.group(1)
                # cmd = ["youtubedown", '--no-mux', url]
                cmd = ["youtubedown", url]
                print(url, cmd)
                result = subprocess.call(cmd)
                if not result:
                    os.unlink(url_file)


if __name__ == "__main__":
    main()

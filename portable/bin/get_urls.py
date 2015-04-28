#!/usr/bin/env python
"""Back up a file or files."""
import os
import re
import subprocess
import logging
import argparse

FORMAT = '%(levelname)s %(lineno)d %(message)s'
logging.basicConfig(format=FORMAT)

def main():
    """Do it."""
    parser = argparse.ArgumentParser()
    parser.add_argument('url_files', nargs='+')
    args = parser.parse_args()
    for url_file in args.url_files:
        lines = [line.strip() for line in open(url_file)]
        for line in lines:
            matches = re.match(r'URL=(.*)', line)
            if matches is not None:
                url = matches.group(1)
                print url
                print ["youtubedown", url]
                result = subprocess.call(["youtubedown", url])
                if not result:
                    os.unlink(url_file)


if __name__ == "__main__":
    main()

import logging
import os
import sys
from argparse import ArgumentParser, Namespace
from signal import signal, SIGPIPE, SIG_DFL
from collections.abc import Callable
from typing import TextIO

logger = logging.getLogger(__name__)


def my_except_hook(exctype, value, traceback):
    if exctype == BrokenPipeError:
        pass
    else:
        sys.__excepthook__(exctype, value, traceback)


class Jmullan:
    GO = True


def handle_signal(signum, frame):
    logger.debug(f"Received signal {signum}")
    if SIGPIPE == signum:
        sys.stderr.close()
        exit(1)
    if not Jmullan.GO:
        logger.debug("Received two signals, so immediately quitting")
        exit(0)
    Jmullan.GO = False


def ignore_broken_pipe_error():
    sys.excepthook = my_except_hook
    signal(SIGPIPE, SIG_DFL)


def stop_on_broken_pipe_error():
    sys.excepthook = my_except_hook
    signal(SIGPIPE, handle_signal)


def open_file_or_stdin(filename: str) -> TextIO:
    if filename == "-":
        return sys.stdin
    else:
        return open(filename, "r")


def read_file_or_stdin(filename: str) -> str:
    if filename == "-":
        return sys.stdin.read()
    else:
        file_size = os.path.getsize(filename)
        with open(filename) as f:
            return f.read(file_size)


def write_to_file_or_stdin(filename: str, contents: str):
    if filename == "-":
        print(contents)
    else:
        with open(filename, "w") as f:
            f.write(contents)


def add_filenames_arguments(parser: ArgumentParser):
    parser.add_argument(
        "filenames",
        nargs="*",
        help=(
            "a list of files; - for stdin; separate arguments from"
            " files with an optional -- ; specifying no files means stdin"
        ),
    )


def get_filenames(args: Namespace):
    filenames = args.filenames or []
    if not filenames:
        filenames.append("-")
    return filenames


def update_in_place(filename: str, changer: Callable[[str], str]):
    contents = read_file_or_stdin(filename)
    new_contents = changer(contents)
    changed = new_contents != contents
    if changed:
        logger.debug("updated file %s\n" % filename)
        write_to_file_or_stdin(filename, new_contents)

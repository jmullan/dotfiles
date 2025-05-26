#!/usr/bin/env -S python-venv --virtualenv dotfiles
import time
from typing import TextIO

from jmullan.cmd import cmd
from jmullan.logging.easy_logging import easy_initialize_logging
import logging
import requests
from jmullan.logging.helpers import logging_context_from_args

logger = logging.getLogger(__name__)


class Main(cmd.TextIoProcessor):
    bases: list[str]

    def __init__(self):
        super().__init__()
        self.parser.add_argument(
            "-r",
            "--regex",
            dest="regex",
            action="store_true",
            default=False,
            help="treat the find string as a regex",
        )
        self.parser.add_argument(
            "--base", dest="bases", action="append", help="Use these base urls"
        )

    def setup(self):
        super().setup()
        if self.args.verbose:
            easy_initialize_logging("DEBUG")
        else:
            easy_initialize_logging()
        self.bases = self.args.bases
        logger.info(f"Using base urls {self.bases}")

    def process_file_handle(self, filename: str, file_handle: TextIO):
        for line in file_handle:
            if not cmd.Jmullan.GO:
                break
            stripped = line.strip()
            parts = stripped.split(" ", 1)
            if len(parts) != 2:
                continue
            if parts[0] != "GET":
                continue
            path = parts[1]
            if not len(path):
                continue
            if self.bases:
                for base in self.bases:
                    full_url = f"{base}{path}"
                    self.get(full_url)
            else:
                full_url = path
                self.get(full_url)

    @logging_context_from_args("full_url")
    def get(self, full_url: str):
        headers = {"User-Agent": "jmullan manual testing", "Accept": "application/json"}
        start = time.perf_counter()
        try:
            response = requests.get(full_url, headers=headers)
        except Exception:
            logger.exception("oh no")
            return
        status_code = response.status_code
        size = nice_bytes(len(response.content))
        end = time.perf_counter()
        try:
            json = response.json()
            had_content = bool(json)
            could_parse_json = True
        except Exception:
            had_content = False
            could_parse_json = False
        content_type = response.headers.get("Content-Type")
        if could_parse_json and had_content:
            disposition = f"of {content_type} as json"
        elif could_parse_json:
            disposition = f"of {content_type} as empty json"
        else:
            disposition = f"of {content_type} as not json"

        duration = abs(end - start)
        duration_label = nice_duration(duration)
        message = f"{status_code} Fetched {size} {disposition} in {duration_label}"
        if status_code >= 500:
            logger.error(message)
        elif status_code >= 400 and status_code != 404:
            logger.warning(message)
        elif duration < 0.250:
            logger.debug(message)
        elif duration < 1:
            logger.info(message)
        else:
            logger.warning(message)


def nice_duration(seconds: float) -> str:
    segments = {"d": 86400, "h": 3600, "m": 60, "s": 1}

    result = []
    for name, size in segments.items():
        value, seconds = divmod(seconds, size)
        if value:
            result.append(f"{value}{name}")

    if seconds:
        ms = round(seconds * 1000, 0)
        if ms:
            ms = f"{ms}".removesuffix(".0")
            result.append(f"{ms}ms")
        else:
            ns = round(seconds * 1000 * 1000, 0)
            if ns:
                ns = f"{ns}".removesuffix(".0")
                result.append(f"{ns}ns")
    if result:
        return ", ".join(result)
    else:
        return "0s"


def nice_bytes(byte_count: int) -> str:
    labels = ["k", "M", "G", "T"]
    label = "b"
    for multiplier in labels:
        if byte_count > 1000:
            label = multiplier
            byte_count = round(byte_count / 1000, 1)
        else:
            break
    return f"{byte_count}{label}"


if __name__ == "__main__":
    Main().main()

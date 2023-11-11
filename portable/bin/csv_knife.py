#!/usr/bin/env python-venv
import csv
import json
import sys
from argparse import ArgumentParser
from string import Template

DIALECT_OPTIONS = ["excel", "excel-tab", "unix", "sniff", "presto", "rovi"]

csv.register_dialect(
    "presto",
    delimiter=",",
    quotechar='"',
    escapechar=None,
    doublequote=True,
    skipinitialspace=True,
    lineterminator="\r\n",
    quoting=csv.QUOTE_ALL,
    strict=True,
)
csv.register_dialect(
    "rovi",
    delimiter="\1",
    quotechar='"',
    escapechar=None,
    doublequote=True,
    skipinitialspace=True,
    lineterminator="\2",
    quoting=csv.QUOTE_ALL,
    strict=False,
)


def zip_header(header: list, row: list):
    field_count = max(len(header), len(row))
    zipped = {}
    for x in range(0, field_count):
        if x < len(header):
            h = header[x]
        else:
            h = f"_{x}"

        v = None
        if x < len(row):
            v = row[x]
        zipped[h] = v
    return zipped


def chunker(file_handle, newline: str, chunk_size: int = 4096):
    prior_chunk = ""
    x = 1
    while True:
        x += 1
        chunk = file_handle.read(chunk_size)
        if chunk is None:
            break
        if len(chunk) == 0:
            break
        # first peel off any leading newlines
        while chunk.startswith(newline) and len(chunk) > 0:
            yield prior_chunk
            prior_chunk = ""
            chunk = chunk[1:]
        ends = []
        # now collect any trailing newlines
        while chunk.endswith(newline) and len(chunk) > 0:
            ends.append(newline)
            chunk = chunk[:-1]
        # at this point we have a chunk with no leading or trailing
        # newlines. It may or may not have newlines inside of it
        if newline not in chunk:
            if ends:
                # all the newlines were at the end. Use one of them to
                # clear out anything in a prior chunk and the current chunk
                yield prior_chunk + chunk
                prior_chunk = ""
                while ends:
                    # peel off any remaining newlines
                    yield ""
                    ends = ends[1:]
        else:
            # this chunk has newlines inside of it
            parts = chunk.split(newline)
            while len(parts) > 1:
                # peel off all but the last line of the chunk
                yield prior_chunk + parts[0]
                prior_chunk = ""
                parts = parts[1:]
            while len(ends) > 0:
                # there is at least one trailing newline
                if len(parts) > 0:
                    # there is at least one part left
                    yield prior_chunk + parts[0]
                    prior_chunk = ""
                    parts = parts[1:]
                else:
                    # there are no more parts, so clear out any
                    # prior chunks and ends
                    yield prior_chunk
                    prior_chunk = ""
                ends = ends[1:]
            if len(parts) > 0:
                prior_chunk = prior_chunk + parts[0]
    if len(prior_chunk):
        yield prior_chunk


def process(csvfile, dialect_name, has_header: bool, template=None):
    compiled_template = Template(template)

    if dialect_name == "sniff":
        dialect_name = csv.Sniffer().sniff(csvfile.read(1024))
        if template == "sniff":
            print(dialect_name.__dict__)
            exit(0)
        csvfile.seek(0)

    dialect = csv.get_dialect(dialect_name)
    reader = csv.reader(chunker(csvfile, dialect.lineterminator), dialect=dialect_name)
    if has_header:
        header = next(reader)
    else:
        header = []
    try:
        for csv_row in reader:
            if csv_row is None:
                break
            d = zip_header(header, csv_row)
            if template is None:
                print(d)
            elif template == "json":
                print(json.dumps(d))
            elif template == "csv":
                writer = csv.writer(sys.stdout)
                writer.writerow(csv_row)
            else:
                print(compiled_template.substitute(d))

    except BrokenPipeError:
        sys.stderr.close()
        exit(0)


def main():
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument(
        "--has-header",
        dest="has_header",
        action="store_true",
        default=False,
        help="The csv has a header",
    )
    parser.add_argument(
        "--dialect", dest="dialect", default=None, choices=DIALECT_OPTIONS
    )
    parser.add_argument(
        "--encoding", dest="encoding", default="utf-8-sig", choices=DIALECT_OPTIONS
    )
    parser.add_argument(
        "--sniff-only", dest="sniff", action="store_true", default=False
    )
    parser.add_argument(
        "-t", "--template", dest="template", default=None, help="The commands to run"
    )
    parser.add_argument(
        "-f",
        "--filename",
        dest="filename",
        default=None,
        help="A filename to process (default stdin)",
    )
    args = parser.parse_args()

    dialect = options.dialect
    if options.sniff:
        template = "sniff"
    elif dialect is None:
        dialect = "unix"

    template = options.template
    if (template is None or not len(template)) and dialect == "sniff":
        template = "sniff"

    if options.filename:
        with open(options.filename, "r", encoding=options.encoding) as file_handle:
            process(file_handle, dialect, options.has_header, template=template)
    else:
        if dialect == "sniff" and template != "sniff":
            raise Exception(
                "Cannot sniff stdin and perform other actions besides sniff"
            )
        process(sys.stdin, dialect, options.has_header, template=template)


if __name__ == "__main__":
    main()

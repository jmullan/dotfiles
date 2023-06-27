#!python3.10
import csv
import json
import sys
from argparse import ArgumentParser
from string import Template

DIALECT_OPTIONS = [
    "excel",
    "excel-tab",
    "unix",
    "sniff",
    "presto",
    "rovi"
]

csv.register_dialect(
    "presto",
    delimiter=',',
    quotechar='"',
    escapechar=None,
    doublequote=True,
    skipinitialspace=True,
    lineterminator="\r\n",
    quoting=csv.QUOTE_ALL,
    strict=True
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
    strict=False
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
    prior_chunk = ''
    while True:
        chunk = file_handle.read(chunk_size)
        while chunk.startswith(newline) and len(chunk) > 0:
            yield prior_chunk
            prior_chunk = ''
            chunk = chunk[1:]
        ends = []
        while chunk.endswith(newline) and len(chunk) > 0:
            ends.append(newline)
            chunk = chunk[:-1]
        if newline not in chunk:
            prior_chunk = prior_chunk + chunk
        else:
            parts = chunk.split(newline)
            while len(parts) > 1:
                yield parts[0]
                parts = parts[1:]
            while len(ends) > 0:
                if len(parts) > 0:
                    yield parts[0]
                    parts = parts[1:]
                else:
                    yield ''
            if len(parts) > 0:
                prior_chunk = prior_chunk + parts[0]



def process(csvfile, dialect_name, has_header: bool, template=None):
    compiled_template = Template(template)

    if dialect_name == 'sniff':
        dialect_name = csv.Sniffer().sniff(csvfile.read(1024))
        if template == 'sniff':
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
            d = zip_header(header, csv_row)
            if template is None:
                print(d)
            elif template == 'json':
                print(json.dumps(d))
            else:
                print(compiled_template.substitute(d))

    except BrokenPipeError:
        sys.stderr.close()
        exit(0)


def main():
    parser = ArgumentParser()
    parser.add_argument('-v', '--verbose', dest='verbose',
                        action='store_true', default=False,
                        help='verbose is more verbose')
    parser.add_argument('--has-header', dest="has_header", action='store_true', default=False, help="The csv has a header")
    parser.add_argument('--dialect', dest="dialect", default=None, choices=DIALECT_OPTIONS)
    parser.add_argument('--encoding', dest="encoding", default='utf-8-sig', choices=DIALECT_OPTIONS)
    parser.add_argument('--sniff-only', dest="sniff", action='store_true', default=False)
    parser.add_argument('-t', '--template', dest='template', default=None, help="The commands to run")
    parser.add_argument('-f', '--filename', dest='filename', default=None, help="A filename to process (default stdin)")
    options = parser.parse_args()

    dialect = options.dialect
    if options.sniff:
        template = 'sniff'
    elif dialect is None:
        dialect = 'unix'

    template = options.template
    if (template is None or not len(template)) and dialect == 'sniff':
        template = 'sniff'

    if options.filename:
        with open(options.filename, 'r', encoding=options.encoding) as file_handle:
            process(file_handle, dialect, options.has_header, template=template)
    else:
        if dialect == 'sniff' and template != 'sniff':
            raise Exception("Cannot sniff stdin and perform other actions besides sniff")
        process(sys.stdin, dialect, options.has_header, template=template)


if __name__ == "__main__":
    main()

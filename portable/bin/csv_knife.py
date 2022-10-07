#!python3.10
import csv
import sys
from argparse import ArgumentParser
from string import Template

DIALECT_OPTIONS = [
    "excel",
    "excel-tab",
    "unix",
    "sniff",
    "presto"
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


def process(csvfile, dialect, has_header: bool, template=None):
    compiled_template = Template(template)

    if dialect == 'sniff':
        dialect = csv.Sniffer().sniff(csvfile.read(1024))
        if template == 'sniff':
            print(dialect.__dict__)
            exit(0)
        csvfile.seek(0)

    reader = csv.reader(csvfile, dialect=dialect)
    if has_header:
        header = next(reader)
    else:
        header = []
    try:
        for csv_row in reader:
            d = zip_header(header, csv_row)
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
        with open(options.filename, 'r') as file_handle:
            process(file_handle, dialect, options.has_header, template=template)
    else:
        if dialect == 'sniff' and template != 'sniff':
            raise Exception("Cannot sniff stdin and perform other actions besides sniff")
        process(sys.stdin, dialect, options.has_header, template=template)


if __name__ == "__main__":
    main()

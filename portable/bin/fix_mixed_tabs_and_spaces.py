#!/usr/bin/env python3
import difflib
import os
import re
from argparse import ArgumentParser


def main():
    """Strip $Id$ and stuff."""
    parser = ArgumentParser()
    parser.add_argument('-v', '--verbose', dest='verbose',
                        action='store_true', default=False,
                         help='verbose is more verbose')
    parser.add_argument('-b', '--', dest='verbose',
                        action='store_true', default=False,
                         help='verbose is more verbose')
    parser.add_argument('filenames', nargs='+', help='filenames to process')
    options = parser.parse_args()
    options = options.__dict__
    verbose = options.get('verbose')

    for filename in options['filenames']:
        if verbose:
            print('checking', filename)
        filesize = os.path.getsize(filename)
        with open(filename, 'rb') as f:
            original_contents = f.read(filesize)
        if verbose and (0xD in original_contents):
            print('Found \r in contents')
        original_contents = original_contents.decode('UTF8')
        contents = original_contents
        lines = contents.split('\n')
        stripped_lines = [line.rstrip() for line in lines]
        if verbose and (lines != stripped_lines):
            print('Trimmed line endings')
        contents = '\n'.join(stripped_lines)
        if not contents.endswith('\n'):
            contents = contents + '\n'
        if verbose and (contents != original_contents):
            print("something changed")

        tabs = 0
        spaces = 0
        use = "spaces"
        for line in stripped_lines:
            if line.startswith('\t'):
                tabs += 1
            if line.startswith(' '):
                spaces += 1
        if tabs > spaces:
            if verbose:
                print('using tabs')
            use = 'tabs'
        if spaces == tabs and verbose:
            print('Same number of tabs and spaces: %s' % spaces)

        if use == 'spaces':
            replace_patterns = [
                (r'\t', ' ' * 4),
                (r'\t ', ' ' * 5),
                (r' \t', ' ' * 5),
                (r'[ \t]+\n', '\n'),
            ]
        else:
            replace_patterns = [
                (r' {4}\t', '\t\t'),
                (r'\t {4}', '\t\t'),
                (r' {3}\t', '\t\t'),
                (r'\t {3}', '\t\t'),
                (r' {3}\t', '\t\t'),
                (r'\t {3}', '\t\t'),
                (r' \t', '\t\t'),
                (r'[ \t]+\n', '\n'),
            ]

        for pattern, replacement in replace_patterns:
            new_contents = ''
            while new_contents != contents:
                if verbose:
                    print("replacing %r with %r" % (pattern, replacement))
                new_contents = contents
                contents = re.sub(pattern, replacement, new_contents)
                if verbose and (contents != new_contents):
                    print("replaced %r with %r" % (pattern, replacement))

        changed = contents != original_contents
        if changed:
            if verbose:
                print('updated file %s' % filename)
                original_lines = {i: x for i, x in enumerate(original_contents.split('\n'))}
                updated_lines = {i: x for i, x in enumerate(contents.split('\n'))}
                limit = max(len(original_lines), len(updated_lines))
                for i in range(0, limit):
                    original = original_lines.get(i)
                    updated = updated_lines.get(i)
                    if original is None:
                        print('+ %r' % updated)
                    elif updated is None:
                        print('- %r' % original)
                    elif original != updated:
                        print('- %r' % original)
                        print('+ %r' % updated)
                    else:
                        print('  %r' % original)
            with open(filename, 'w') as f:
                f.write(contents)
        else:
            if verbose:
                print('did not update file %s' % filename)


if __name__ == "__main__":
    main()

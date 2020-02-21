#!/usr/bin/env python
import os
import re
import sys
from optparse import OptionParser


def main():
    """Strip $Id$ and stuff."""
    changed = False
    parser = OptionParser()
    parser.add_option('-v', '--verbose', dest='verbose',
                      action='store_true', default=False,
                      help='verbose is more verbose')
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get('verbose')

    for filename in args:
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            original_contents = f.read(filesize)
            contents = original_contents

        plaintext_replacements = [
            ('To change body of implemented methods use File | Settings | File Templates.', ''),
            ('To change this template use File | Settings | File Templates.', ''),
            ('Created with IntelliJ IDEA.', '')
        ]

        for find, replace in plaintext_replacements:
            contents = contents.replace(find, replace)

        delete_patterns = [
            r'[\n\r]*\s*\*\s*@version\s*\$Id\$ *',
            r'[\n\r]*\s*\*\s*\$Id\$ *',
            r'[\n\r]*\s*<!--\$Id\$-->',
            r'[\n\r]*\s*\$Id\$ *',

            r'[\n\r]*\s*\*\s*Last\s*edited:\s*\$Date\$ *',
            r'[\n\r]*\s*\*\s*\$Date\$ *',
            r'[\n\r]*\s*<!--\s*\$Date\s*\$-->',
            r'[\n\r]*\s*\$Date\$ *',

            r'[\n\r]*\s*\*\s*\$Author\$\s*',
            r'[\n\r]*\s*<!--\$Author\$-->',
            r'[\n\r]*\s*\$Author\$ *',

            r'/\*+([\n\r]*\s*\**\s*)\*/\s*',

            r'[\n\r]*\s*\*\s*@version \$Id :\$',
            r'[\n\r]*\s*\*\s*@Version\s*\:?\s*$',
            r'[\n\r]*\s*\*\s+$',
        ]
        for pattern in delete_patterns:
            contents = re.sub(pattern, '', contents)

        newline_patterns = [
            r'[\n\r]*\s*\* @return\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[A-Za-z]*Exception\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[A-Za-z]*Exception\s*error\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[A-Za-z]*Exception\s*exception\s*[\n\r]+',
            r'\s*[\n\r]+\s*\*\s*@param\s+[A-Za-z]*\s*[\n\r]+',
            r'\s*//\s*[\n\r]+'
        ]
        for pattern in newline_patterns:
            contents = re.sub(pattern, '\n', contents)

        replace_patterns = [
            (r'[\n\r]*\s*\*\s*([\n\r]\s*\*/)', r'\1'),
            (r'/\*\s*[\n\r]+\s\**\s*[\n\r]+', '/*\n'),
            (r'/\*\*+\s*[\n\r]+\s\**\s*[\n\r]+', '/**\n'),
        ]
        for pattern, replacement in replace_patterns:
            contents = re.sub(pattern, replacement, contents)


        changed = contents != original_contents
        if changed:
            if verbose:
                sys.stdout.write('updated file %s' % filename)
            with open(filename, 'w') as f:
                f.write(contents)


if __name__ == "__main__":
    main()

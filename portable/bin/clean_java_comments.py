#!/usr/bin/env python3.8
import os
import re
import sys
from argparse import ArgumentParser


def main():
    """Strip $Id$ and stuff."""
    changed = False
    parser = ArgumentParser()
    parser.add_argument('-v', '--verbose', dest='verbose',
                        action='store_true', default=False,
                         help='verbose is more verbose')
    parser.add_argument('filenames', nargs='+', help='filenames to process')
    options = parser.parse_args()
    options = options.__dict__
    verbose = options.get('verbose')

    for filename in options['filenames']:
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            original_contents = f.read(filesize)
            contents = original_contents

        plaintext_replacements = [
            ('To change body of implemented methods use File | Settings | File Templates.', ''),
            ('To change this template use File | Settings | File Templates.', ''),
            ('Created with IntelliJ IDEA.', ''),
            ('Created with IntelliJ IDEA', ''),
            ('Created by IntelliJ IDEA.', ''),
            ('Created by IntelliJ IDEA', ''),
        ]

        for find, replace in plaintext_replacements:
            contents = contents.replace(find, replace)

        delete_strings = [
            "// Accessors",
            "// Accessors and Mutators",
            "// Accessors, Mutators, etc.",
            "// Class Members",
            "// Class Methods",
            "// Constructors",
            "// Constructors and Initializers",
            "// Creation, Modification (user, timestamp) Methods",
            "// Data Member",
            "// Data Members",
            "// Dump and Debug helper methods",
            "// Getters and setters",
            "// Implement abstract methods",
            "// Inner Classes",
            "// Inner classes",
            "// Logger helpers",
            "// Logging Methods",
            "// Members",
            "// Method overridable by implementing classes",
            "// Methods defined by the implementing classes",
            "// Methods overridable in the derived classes",
            "// Methods overridden by the derived classes",
            "// Methods overridden by the implementing class",
            "// Methods overriden from the super classes",
            "// Mutators and Accesors",
            "// Object Data",
            "// Object Dump Methods",
            "// Playlist generation",
            "// Private Utility methods",
            "// Private and Utility Methods",
            "// Private and utility methods",
            "// Private getters and setters for Hibernate",
            "// Private methods",
            "// Private methods for Hibernate",
            "// Public Accessors and Mutators",
            "// Redirections to the invoking action",
            "// Request Params helpers",
            "// Request Processing",
            "// Shared data suppliers",
            "// Static Data",
            "// Static Data and Members",
            "// Static Members",
            "// Static Members (test data definitions)",
            "// Static Methods",
            "// Static members",
            "// Static methods",
            "// Status check methods",
            "// Super class overrides",
            "// Test cases",
            "// Tests",
            "// Timing Methods",
            "// UI methods moved here when doing the LDAP refactoring",
            "// Utilily and standard overrides",
            "// Utilitly and standard overrides",
            "// Utilitly and standard overrides/",
            "// Utility and private Accessors and Mutators",
            "// Utility and test methods",
            "// Utility method",
            "// Utility methods",
            "// Utility methods overrides",
            "// VM Property helper methods",
            "// private and utility methods",
            "// RCSInfo: $Id$",
        ]

        delete_strings.sort(key=len, reverse=True)
        for delete_string in delete_strings:
            delete_string = delete_string.replace('\\', '\\\\')
            delete_string = delete_string.replace('$', '\\$')
            delete_string = re.sub(r'\s+', r'\\s+', delete_string)
            pattern = r'[\n\r]*\s*' + delete_string + '[\n\r]+'
            contents = re.sub(pattern, '\n', contents)

        delete_patterns = [
            r'[\n\r]*\s*RCSInfo:\s*\$Id\$ *',
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
            r'[\n\r]*\s*\*+\s+$',
        ]
        for pattern in delete_patterns:
            contents = re.sub(pattern, '', contents)

        newline_patterns = [
            r'[\n\r]*\s*\*\s*-+\s*[\n\r]+',
            r'[\n\r]*\s*//\s*=+\s*[\n\r]+',
            r'[\n\r]*\s*//\s*-+\s*[\n\r]+',
            r'[\n\r]*\s*\* @return\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[A-Za-z]*Exception\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[A-Za-z]*Exception\s*error\s*[\n\r]+',
            r'[\n\r]*\s*\* @throws\s*[A-Za-z]*Exception\s*exception\s*[\n\r]+',
            r'\s*[\n\r]+\s*\*\s*@param\s+[A-Za-z]*\s*[\n\r]+',
            r'[ \t]*//+\s*[\n\r]+'
        ]
        for pattern in newline_patterns:
            contents = re.sub(pattern, '\n', contents)

        replace_patterns = [
            (r'[\n\r]*\s*\*+\s*(?P<ws>[\n\r]\s*\*/)', r'\g<ws>'),
            (r'/\*\s*[\n\r]+\s\**\s*[\n\r]+', '/*\n'),
            (r'/\*\*+\s*[\n\r]+\s\**\s*[\n\r]+', '/**\n'),
            (r'[\n\r]+\s*[\n\r](?P<ws> *)\*/', '\n\g<ws>*/')
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

#!/usr/bin/env -S python-venv --virtualenv dotfiles
import logging
import re
import sys

from jmullan.cmd import cmd
from jmullan.logging.easy_logging import easy_initialize_logging

logger = logging.getLogger(__name__)

def clean_line_comment(comment):
    comment = comment.rstrip()
    matches = re.match(
        r"^(\s*)(//)([\\s]*)(.*)", comment, flags=re.DOTALL | re.MULTILINE
    )
    if not matches:
        return comment
    groups = matches.groups()
    if not groups:
        return comment
    # any extra spaces and slashes after the // is junk
    indent, slashes, junk, line = groups
    line = clean_line(line).strip()
    if len(line):
        return "%s%s %s" % (indent, slashes, line)
    return ""


def clean_block_comment(comment):
    matches = re.match(
        r"(\n*)([ \t]*)(/\*+[ \*]*)(.*)(\*/)", comment, flags=re.DOTALL | re.MULTILINE
    )
    if not matches:
        return comment
    groups = matches.groups()
    if not groups:
        return comment
    # ('', '/*', '\n * Copyright (c) 2016 Pandora Media, Inc.\n * @version $Id:  $\n ', '*/')
    newlines, indent, opener, body, closer = groups
    while body.endswith("*") or body.endswith(" "):
        # remove any extra * characters from the body
        body = body[:-1]
    opener = opener[:3]  # standardize to /* or /**
    body = body.strip("\n")
    body_lines = body.split("\n")
    cleaned_lines = []
    for line in body_lines:
        if line.startswith(indent):
            line = line[len(indent) :]
        # if there is a star or stars at the start of the line, trim off everything up to and including the star
        # we insert one more space because we want to trim off one more space
        line = re.sub(r"^\s*\*+ ?", " ", line)

        # We are cutting off the final space
        # /*
        #  * <- before this star

        # Otherwise
        # /*
        # --There is no important leading space here
        # --__The underscores are where important leading space would be
        if line.startswith(" "):
            line = line[1:]
        # at this point all lines should be even with the indent + 1, and any further white space is presumably intentional
        cleaned_line = clean_line(line)
        cleaned_lines.append(cleaned_line)
    cleaned_lines = trim_empty_lines(cleaned_lines)
    if cleaned_lines:
        if len(cleaned_lines) == 1:
            if opener == "/**":
                return "%s%s%s %s %s" % (
                    newlines,
                    indent,
                    opener,
                    cleaned_lines[0].strip(),
                    closer,
                )
            else:
                return "%s%s// %s" % (newlines, indent, cleaned_lines[0].strip())
        else:
            clean_body = "".join(
                ("\n%s * %s" % (indent, cleaned_line)).rstrip()
                for cleaned_line in cleaned_lines
            )
            return "%s%s%s%s\n%s %s" % (
                newlines,
                indent,
                opener,
                clean_body,
                indent,
                closer,
            )
    else:
        return newlines


def trim_empty_lines(lines):
    trimmed_body = []
    blank_lines = []
    for line in lines:
        if re.search(r"\S", line):
            trimmed_body.extend(blank_lines)
            trimmed_body.append(line)
            blank_lines = []
        elif len(blank_lines) < 1:
            blank_lines.append(line)
    return trimmed_body


def clean_line(line):
    delete_patterns = [
        r"^\s+\*+\s*$" r"\s*RCSInfo:\s*\$Id\$\s*",
        r"\s*@version\s*\$Id\$\s*",
        r"\s*\$Id\$\s*",
        r"\s*<!--\$Id\$-->",
        r"\s*\$Id\$\s*",
        r"\s*Last\s*edited:\s*\$Date\$\s*",
        r"\s*\$Date\$\s*",
        r"\s*<!--\s*\$Date\s*\$-->",
        r"\s*\$Date\$\s*",
        r"\s*\$Author\$\s*",
        r"\s*<!--\$Author\$-->",
        r"\s*\$Author\$\s*",
        r"\s*@version\s\$Id\s*:\s*\$",
        r"\s*@Version\s*\:?\s*$",
        r"\s*\*+\s+$",
        r"^\s*-+\s*$",
        r"^\s*=+\s*$",
        r"^\s*-+\s*$",
        r"^\s*/+\s*$",
        r"^\s*@return\s*$",
        r"^\s*@throws\s*$",
        r"^\s*@throws\s*E\s*$",
        r"^\s*@throws\s*[A-Za-z]*Exception\s*$",
        r"^\s*@throws\s*[A-Za-z]*Exception\s*error\s*$",
        r"^\s*@throws\s*[A-Za-z]*Exception\s*exception\s*$",
        r"^\s*@param\s+[A-Za-z][A-Za-z0-9]*\s*$",
        r"^\s*@param\s+<[A-Za-z0-9]*>\s*$",
    ]
    for pattern in delete_patterns:
        line = re.sub(pattern, "", line)
    return line.rstrip()


def clean(contents):
    plaintext_replacements = [
        (
            "To change body of implemented methods use File | Settings | File Templates.",
            "",
        ),
        ("To change this template use File | Settings | File Templates.", ""),
        ("Created with IntelliJ IDEA.", ""),
        ("Created with IntelliJ IDEA", ""),
        ("Created by IntelliJ IDEA.", ""),
        ("Created by IntelliJ IDEA", ""),
    ]

    for find, replace in plaintext_replacements:
        contents = contents.replace(find, replace)

    delete_strings = [
        "// Accessors and Mutators",
        "// Accessors, Mutators, etc.",
        "// Accessors",
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
        delete_string = delete_string.replace("\\", "\\\\")
        delete_string = delete_string.replace("$", "\\$")
        delete_string = re.sub(r"\s+", r"\\s+", delete_string)
        pattern = r"[\n\r]*\s*" + delete_string + "[\n\r]+"
        contents = re.sub(pattern, "\n", contents)
    return contents


def clean_block_comments(contents: str) -> str:
    block_comments = re.findall(
        r"(?:\n|^)\s*/\*.*?\*/", contents, flags=re.DOTALL | re.MULTILINE
    )
    for block_comment in block_comments:
        updated_comment = clean_block_comment(block_comment)
        if block_comment != updated_comment:
            contents = contents.replace(block_comment, updated_comment)
    return contents


def clean_line_comments(contents: str) -> str:
    line_comments = re.findall(r"^\s*//.*$", contents, flags=re.MULTILINE)
    for line_comment in line_comments:
        cleaned_line_comment = clean_line_comment(line_comment)
        if line_comment != cleaned_line_comment:
            safe_find = "^%s$" % re.escape(line_comment)
            contents = re.sub(
                safe_find, cleaned_line_comment, contents, flags=re.MULTILINE
            )
    return contents


def process_contents(original_contents: str) -> str:
        if "This file was generated by the JavaTM Architecture" in original_contents:
            return original_contents
        contents = original_contents
        contents = clean_block_comments(contents)
        contents = clean_line_comments(contents)

        contents = clean(contents)

        return contents


class Main(cmd.InPlaceFileProcessor):

    def setup(self):
        super().setup()
        if self.args.verbose:
            easy_initialize_logging("DEBUG", stream=sys.stderr)
        else:
            easy_initialize_logging("INFO", stream=sys.stderr)

    def process_contents(self, contents: str) -> str:
        return process_contents(contents)


if __name__ == "__main__":
    Main().main()

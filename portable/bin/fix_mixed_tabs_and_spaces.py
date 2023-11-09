#!/usr/bin/env python-venv dotfiles
import os
import re
from argparse import ArgumentParser


def main():
    """Strip $Id$ and stuff."""
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
        "-n",
        "--noop",
        dest="noop",
        action="store_true",
        default=False,
        help="Dry run only",
    )
    parser.add_argument(
        "-s",
        "--spaces",
        dest="spaces",
        action="store_true",
        default=False,
        help="Ignore tabs, embrace spaces",
    )
    parser.add_argument(
        "-t",
        "--tabs",
        dest="tabs",
        action="store_true",
        default=False,
        help="Ignore spaces, embrace tabs",
    )
    parser.add_argument("filenames", nargs="+", help="filenames to process")
    options = parser.parse_args()
    options = options.__dict__
    verbose = options.get("verbose")

    for filename in options["filenames"]:
        if verbose:
            print("checking", filename)
        filesize = os.path.getsize(filename)
        with open(filename, "rb") as f:
            original_contents = f.read(filesize)
        if verbose and (0xD in original_contents):
            print("Found \r in contents")
        original_contents = original_contents.decode("UTF8")
        contents = original_contents
        lines = contents.split("\n")
        stripped_lines = [line.rstrip() for line in lines]
        if verbose and (lines != stripped_lines):
            print("Trimmed line endings")
        contents = "\n".join(stripped_lines)
        if not contents.endswith("\n"):
            contents = contents + "\n"
        if verbose and (contents != original_contents):
            print("something changed")

        if options.get("tabs"):
            use = "tabs"
        elif options.get("spaces"):
            use = "spaces"
        else:
            tabs = 0
            spaces = 0
            use = "spaces"
            for line in stripped_lines:
                if line.startswith("\t"):
                    tabs += 1
                if line.startswith(" "):
                    spaces += 1
            if tabs > spaces:
                if verbose:
                    print("using tabs")
                use = "tabs"
            if spaces == tabs and verbose:
                print("Same number of tabs and spaces: %s" % spaces)

        lines = contents.split("\n")
        new_lines = []
        for line in lines:
            matches = re.match(r"^([ \t]+)(.*)", line)

            if matches:
                leading_whitespace = matches.group(1)
                rest_of_line = matches.group(2)
                if use == "spaces" and "\t" in leading_whitespace:
                    leading_whitespace = leading_whitespace.replace("\t", " " * 4)
                elif use == "tabs" and " " in leading_whitespace:
                    leading_whitespace = leading_whitespace.replace(" " * 4, "\t")
                line = f"{leading_whitespace}{rest_of_line}"
            new_lines.append(line)
        contents = "\n".join(lines)

        changed = contents != original_contents
        if changed:
            if verbose:
                print("updated file %s" % filename)
                original_lines = {
                    i: x for i, x in enumerate(original_contents.split("\n"))
                }
                updated_lines = {i: x for i, x in enumerate(contents.split("\n"))}
                limit = max(len(original_lines), len(updated_lines))
                for i in range(0, limit):
                    original = original_lines.get(i)
                    updated = updated_lines.get(i)
                    if original is None:
                        print("+ %r" % updated)
                    elif updated is None:
                        print("- %r" % original)
                    elif original != updated:
                        print("- %r" % original)
                        print("+ %r" % updated)
                    else:
                        print("  %r" % original)
            if not options.get("noop"):
                with open(filename, "w") as f:
                    f.write(contents)
        else:
            if verbose:
                print("did not update file %s" % filename)


if __name__ == "__main__":
    main()

#!/usr/bin/env python-venv
import re
import sys


def main():
    """Receive properties, make configuration"""
    lines = (line.rstrip() for line in sys.stdin)
    lines = (re.sub(r"=.*", "", line) for line in lines)
    lines = (re.sub(r"#.*", "", line) for line in lines)
    lines = (re.sub(r"^ .*", "", line) for line in lines)
    lines = (line.strip() for line in lines)
    lines = set((line for line in lines if len(line)))

    # cat  importer/src/main/resources/config/rovi.properties | sed 's/=.*//' | sed 's/#.*//' | sort -u | awk '{print "@Value(\""$1"\")"; print "String "$1";"; print ""}'
    for line in sorted(lines):
        print('@Value("%s")' % line)
        words = line.split(".")
        camel = words[1] + "".join(x.title() for x in words[2:])

        print("private String %s;" % camel)
        print("")

        getter = "get" + "".join(x.title() for x in words[1:])
        print("public String %s() {" % getter)
        print("    return %s;" % camel)
        print("}")
        print("")


if __name__ == "__main__":
    main()

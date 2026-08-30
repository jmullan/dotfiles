#!/bin/bash
if [ $# -lt 1 ] ; then
    exit 1
fi
if [ ! -f "${1}" ] ; then
    exit 1
fi
awk '
    BEGIN {
        separator = ""
    }
    # Pass 1: clean lines, find variables
    {
        line = $0
        sub(/#.*/, "", line)
        sub(/^[[:space:]]*/, "", line)
        sub(/[[:space:]]*$/, "", line)

        if (line ~ /^\$[A-Za-z_]+=/) {
            name = line
            sub(/=.*/, "", name)

            value = line
            sub(/^[^=]*=/, "", value)

            vars[name] = value
        } else if (line ~ /=/)  {
            extension = line
            sub(/=.*/, "", extension)
            extensions[extension] = line
        }
    }
    END {
        # Pass 2: substitute variables and emit entries.
        for (extension in extensions) {
            line = extensions[extension]
            for (name in vars) {
                gsub("\\" name, vars[name], line)
            }
            printf "%s%s", separator, line
            separator = ":"
        }
    }
' "${1}"

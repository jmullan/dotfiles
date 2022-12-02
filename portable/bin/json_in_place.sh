#!/bin/bash
FILENAME="${1}"
if [ -z "${FILENAME}" ] ; then
    echo "No filename specified"
    exit 1
fi
if [ ! -f "${FILENAME}" ] ; then
    echo "File not found"
    exit 1
fi
TMPFILE=$(mktemp --suffix=.json)
echo "${FILENAME}"
cat "${FILENAME}" | json_pp -json_opt pretty,canonical,utf8 > "${TMPFILE}" && cp "${FILENAME}" "${FILENAME}.bak" && mv "${TMPFILE}" "${FILENAME}"

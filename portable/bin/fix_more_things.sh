#!/bin/bash
trap exit SIGHUP SIGINT SIGTERM
INDENT=`current_editor_config indent_size`
fix_simple_whitespace.sh ${1+"$@"}
unify --quote "'" --in-place ${1+"$@"}
docformatter -i --wrap-summaries 120 --wrap-descriptions 110 --no-blank ${1+"$@"}
autopep8 -i --ignore-local-config --global-config ../.autopep8 --indent-size="$INDENT" --select=E101 ${1+"$@"}

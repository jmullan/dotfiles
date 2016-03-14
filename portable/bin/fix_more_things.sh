#!/bin/bash
trap exit SIGHUP SIGINT SIGTERM
INDENT=`current_editor_config indent_size`
[ `which fix_simple_whitespace` ] && fix_simple_whitespace.sh ${1+"$@"}
[ `which unify` ] && unify --quote "'" --in-place ${1+"$@"}
[ `which docformatter` ] && docformatter -i --wrap-summaries 120 --wrap-descriptions 110 --no-blank ${1+"$@"}
[ `which autopep8` ] && autopep8 -i --ignore-local-config --global-config ../.autopep8 --indent-size="$INDENT" --select=E201,E202,E203,E211,E221,E222,E223,E224,E225,E226,E227,E228,E231,E241,E242,E251,E261,E271,E272,E273,E274 ${1+"$@"}
[ `which autopep8` ] && autopep8 -i --ignore-local-config --global-config ../.autopep8 --indent-size="$INDENT" --select=E101 ${1+"$@"}

#!/bin/bash
trap exit SIGHUP SIGINT SIGTERM
fix_simple_whitespace.sh ${1+"$@"}
unify --quote "'" --in-place ${1+"$@"}
docformatter -i --wrap-summaries 120 --wrap-descriptions 110 ${1+"$@"}

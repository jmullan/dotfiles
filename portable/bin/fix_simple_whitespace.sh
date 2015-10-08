#!/bin/bash
trap exit SIGHUP SIGINT SIGTERM
#fix_c0303.py --only-blank-lines ${1+"$@"}
fix_c0303.py ${1+"$@"}
fix_w391.py ${1+"$@"}
autopep8 -i --ignore-local-config --global-config ../.autopep8 --select E301,E302,E303 ${1+"$@"}
isort ${1+"$@"}
# docformatter -i --wrap-summaries 120 --wrap-descriptions 110 ${1+"$@"}

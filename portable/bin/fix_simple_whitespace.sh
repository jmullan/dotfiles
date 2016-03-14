#!/bin/bash
trap exit SIGHUP SIGINT SIGTERM
#fix_c0303.py --only-blank-lines ${1+"$@"}
[ `which fix_c0303.py` ] && fix_c0303.py ${1+"$@"}
[ `which fix_w391.py` ] && fix_w391.py ${1+"$@"}
[ `which autopep8` ] && autopep8 -i --ignore-local-config --global-config ../.autopep8 --select E301,E302,E303 ${1+"$@"}
[ `which isort` ] && isort ${1+"$@"}
# docformatter -i --wrap-summaries 120 --wrap-descriptions 110 ${1+"$@"}
# unify --quote "'" --in-place ${1+"$@"}

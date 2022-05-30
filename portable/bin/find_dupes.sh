#!/bin/bash
find -not -empty -type f -printf "%p\t%s\n" |\
    sort -t$'\t' -n -k 2 |\
    uniq -d -f 2 --all-repeated |\
    cut -f 1 |\
    xargs -d '\n' md5sum |\
    sort |\
    uniq -w32 -d |\
    cut -c 35-

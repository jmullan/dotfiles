#! /bin/sh

for i
do
    mv "$i" "$i.bak" || exit 1
    xmllint --format "$i.bak" > "$i"
done

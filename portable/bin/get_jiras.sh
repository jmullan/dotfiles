#!/bin/bash
INTERESTING=~/.jiras_interesting
JIRAS_CSV=~/.jiras.csv
if [ -e ".git" ] ; then
    git log | ag '\b[A-Z]+-[0-9]+' -o >> "${INTERESTING}"
fi
sort -u ~/.jiras_interesting > "${INTERESTING}.sorted"
mv "${INTERESTING}.sorted" "${INTERESTING}"
while read -r line || [[ -n "$line" ]] ; do
    FOUND=$(csv_knife "${JIRAS_CSV}" --template '$_0' | grep "${line}" | head -n 1)
    if [ -z "${FOUND}" ] ; then
        echo "missing jira ${line}"
        CSV_LINE=$(jira --ticket "${line}" --format csv --no-forward)
        if [ -z "${CSV_LINE}" ] ; then
            echo "${line} is missing"
            CSV_LINE=$(jira --ticket "${line}" --format csv)
        fi
        if [ -n "${CSV_LINE}" ] ; then
            sleep 5
            echo "found ${line} as ${CSV_LINE}"
            echo "${CSV_LINE}" >> "${JIRAS_CSV}"
        else
            echo "Did not find ${line}"
        fi
        sleep 5
    else
        echo "Already have ${FOUND}"
    fi
done < <(sort -u ~/.jiras_interesting)

# jira --project AESOP --max 304 --no-forward >> ~/.jiras.csv
# jira --project API --max 362 --no-forward >> ~/.jiras.csv
# jira --project CATALOG --max 3374 --no-forward >> ~/.jiras.csv
# jira --project CATSVCS --max 3156 --no-forward >> ~/.jiras.csv
# jira --project CC --max 1145 --no-forward >> ~/.jiras.csv
# jira --project CNTDLVRY --max 3650 --no-forward >> ~/.jiras.csv
# jira --project CONTCONSOL --max 3805 --no-forward >> ~/.jiras.csv
#jira --project CONTENG --max 7247 --no-forward >> ~/.jiras.csv
#jira --project CONTIN --max 10694 --no-forward >> ~/.jiras.csv
#jira --project CREATORS --max 26447 --no-forward >> ~/.jiras.csv
#jira --project METADATA --max 3936 --no-forward >> ~/.jiras.csv
#jira --project MOPS --max 5368 --no-forward >> ~/.jiras.csv
#jira --project PFORA --max 9104 --no-forward >> ~/.jiras.csv
#jira --project PLAYBACK --max 5244 --no-forward >> ~/.jiras.csv
#jira --project RADIO --max 30016 --no-forward >> ~/.jiras.csv
#jira --project RIGHTS --max 688 --no-forward >> ~/.jiras.csv

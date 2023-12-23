#!/bin/bash
argcount=$#
if [ $argcount -eq 0 ] ; then
    echo "No git hook specified" 1>&2
    exit 1
fi
SCRIPT="${1}"
if [ -z "${SCRIPT}" ] ; then
    echo "Empty git hook specified" 1>&2
    exit 1
fi
# take the script and leave the rest of the arguments
shift
HOOK=$(basename "${SCRIPT}")
if [ -z "${HOOK}" ] ; then
    echo "Could not figure out hook name from script" 1>&2
    exit 1
fi
case "${HOOK}" in
    "pre-commit-msg" | "commit-msg" | "pre-commit")
        MESSAGE_FILE="${1}"
        if [ -n "${MESSAGE_FILE}" ] ; then
            if [ -e "${MESSAGE_FILE}" ] ; then
                MESSAGE=$(head -n 1 "${MESSAGE_FILE}")
            fi
        fi
        ;;
esac
allowtimecard=$(git config --bool hooks.allowtimecard)
if [ "$allowtimecard" == "true" ]; then
    git timecard "${HOOK}" "${MESSAGE}"
fi

GIT_DIR=$(git rev-parse --absolute-git-dir 2>/dev/null)
if [ -z "${GIT_DIR}" ] ; then
    echo "Cannot find git dir -- are you sure we are in a git repo?" 1>&2
    exit 1
fi

PROJECT_HOOK="${GIT_DIR}/hooks/${HOOK}"
if [ -n "${PROJECT_HOOK}" ] ; then
    if [ -e "${PROJECT_HOOK}" ] ; then
        "${PROJECT_HOOK}" "${@}"
        exit $?
    fi
fi
bash "${SCRIPT}" "${@}"

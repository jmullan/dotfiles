#!/bin/bash
GIT_DIR="$(git rev-parse --git-dir 2>/dev/null)"
if [ -z "$GIT_DIR" ] ; then
    exit
fi
GIT_PROMPT_ENABLED="$(git config prompt.enabled)"
if [ "${GIT_PROMPT_ENABLED}" != "1" ] ; then
    exit
fi
GIT_STATUS="$(git status -unormal 2>&1)"
if [[ "${GIT_STATUS}" =~ nothing\ to\ commit ]]; then
    GIT_STATUS_COLOR=Color.GREEN
elif [[ "${GIT_STATUS}" =~ nothing\ added\ to\ commit\ but\ untracked\ files\ present ]]; then
    GIT_STATUS_COLOR=Color.CYAN
else
    GIT_STATUS_COLOR=Color.DEFAULT
fi
HEAD_TRACKS="$(git track)"
REBASE="$(git rebasing)"
if [ -f "$GIT_DIR/rebase-merge/head-name" ] ; then
    REBASE_HEAD="$(cat "$GIT_DIR/rebase-merge/head-name")"
fi
if [ -f "$GIT_DIR/MERGE_HEAD" ] ; then
    MERGING="MERGING"
    MERGE_HEAD="$(cat "$GIT_DIR/MERGE_HEAD")"
fi
if [ -n "$HEAD_TRACKS" ] ; then
    ADDED="$(git diff --numstat "${HEAD_TRACKS}" | total)"
    DELETED="$(git diff --numstat "${HEAD_TRACKS}" | awk '{print $2}' | total)"
else
    ADDED=0
    DELETED=0
fi
GROSS="$(expr "${ADDED}" + "${DELETED}")"
NET="$(expr "${ADDED}" - "${DELETED}")"
# ORIGIN_DIFF_STATS=$(echo -n "+${ADDED} -${DELETED} =${NET} ($GROSS)")
ORIGIN_DIFF_STATS=$(echo -n "+${ADDED} -${DELETED}")
echo -n " ["
if [ -n "$REBASE" ]; then
    echo -n " $REBASE"
fi
if [ -n "$REBASE_HEAD" ]; then
    echo -n " $REBASE_HEAD"
fi
if [ -n "$MERGING" ]; then
    echo -n " $MERGING"
fi
if [ -n "$MERGE_HEAD" ]; then
    echo -n " $MERGE_HEAD";
fi
HEAD_NAME="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
MAIN_NAME=$(git config --get gitflow.branch.main)
if [ -z "${MAIN_NAME}" ] ; then
    MAIN_NAME=$(git main)
fi
HEAD_TRACKS=$(git rev-parse --abbrev-ref "${HEAD_NAME}"@{upstream})
MAIN_TRACKS=$(git rev-parse --abbrev-ref "${MAIN_NAME}"@{upstream})

if [ -z "${HEAD_TRACKS}" ] && [ -n "${MAIN_NAME}" ]; then
    HEAD_TRACKS="${MAIN_NAME}"
fi
if [ -n "${HEAD_TRACKS}" ]; then
    SHORT_HEAD_TRACKS=$(echo "${HEAD_TRACKS}" | sed 's#^origin/#o/#' | sed 's#^upstream/#up/#')
    BEHIND="$(git behind "${HEAD_TRACKS}" || echo 0)"
    AHEAD="$(git ahead "${HEAD_TRACKS}" || echo 0)"
    if [ -n "${HEAD_NAME}" ] ; then
        if [ -z "${SHORT_HEAD_TRACKS##*/${HEAD_NAME}}" ] ; then
            ansi Color.WHITE
            echo -n "${SHORT_HEAD_TRACKS%/${HEAD_NAME}}"
            ansi Color.DEFAULT
            echo -n "/"
            ansi "${GIT_STATUS_COLOR}"
            echo -n "${HEAD_NAME}"
            ansi Color.DEFAULT
            if [ "${BEHIND}" -gt 0 ] ; then
                ansi Color.RED
                echo -n "~${BEHIND}"
                ansi Color.DEFAULT
            fi
            if [ "${AHEAD}" -gt 0 ] ; then
                ansi Color.GREEN
                echo -n "+${AHEAD}"
                ansi Color.DEFAULT
            fi
        else
            ansi Color.WHITE
            echo -n "${SHORT_HEAD_TRACKS}"
            ansi Color.DEFAULT
            if [ "${BEHIND}" -gt 0 ] ; then
                ansi Color.RED
                echo -n "~${BEHIND}"
                ansi Color.DEFAULT
            fi
            if [ "${AHEAD}" -gt 0 ] ; then
                ansi Color.GREEN
                echo -n "+${AHEAD}"
                ansi Color.DEFAULT
            fi
            ansi "${GIT_STATUS_COLOR}"
            echo -n " ${HEAD_NAME}"
            ansi Color.DEFAULT
        fi
    fi
else
    if [ -n "${HEAD_NAME}" ]; then
        ansi "${GIT_STATUS_COLOR}"
        echo -n "${HEAD_NAME}"
        ansi Color.DEFAULT
    fi
fi
if [[ "${GIT_STATUS}" =~ Changes\ to\ be\ committed ]] ; then
    ansi Color.WHITE
    echo -n " staged"
    ansi Color.DEFAULT
fi
if [[ "${GIT_STATUS}" =~ not\ staged ]] ; then
    ansi Color.YELLOW
    echo -n " unstaged"
    ansi Color.DEFAULT
fi
if [[ "${GIT_STATUS}" =~ ntracked ]] ; then
    ansi Color.RED
    echo -n " untracked"
    ansi Color.DEFAULT
fi
if [ "${GROSS}" -gt 0 ]; then
    echo -n " ${ORIGIN_DIFF_STATS}"
    # git diff-graph
fi
FEATURE=$(echo "${HEAD_NAME}" | grep ^feature/)
REMOTE=$(git remote)
DEVELOP_NAME=$(git config --get gitflow.branch.develop)
if [ -z "${DEVELOP_NAME}" ] ; then
    DEVELOP_NAME=develop
fi
DEVELOP_REV=$(git rev-parse --abbrev-ref "${DEVELOP_NAME}" 2>&1 | grep 'unknown revision or path not in the working tree')
if [ -n "${DEVELOP_REV}" ] ; then
    DEVELOP_NAME="${MAIN_NAME}"
fi
if [ -n "$REMOTE" ] ; then
    ORIGIN_DEVELOP=$(git for-each-ref --format='%(refname:short)' refs/heads refs/remotes/origin | grep "^origin/${DEVELOP_NAME}\$")
    if [ -n "$FEATURE" ] && [ -n "$ORIGIN_DEVELOP" ] && [ "$HEAD_TRACKS" != "$ORIGIN_DEVELOP" ] ; then
        BEHIND=$(git behind "origin/${DEVELOP_NAME}")
        AHEAD=$(git ahead "origin/${DEVELOP_NAME}")
        if [ "${BEHIND}" -gt 0 ] || [ "${AHEAD}" -gt 0 ] ; then
            echo -n " ( o/${DEVELOP_NAME}"
        fi
        if [ "${AHEAD}" -gt 0 ] ; then
            echo -n " +${AHEAD}"
        fi
        if [ "${BEHIND}" -gt 0 ] ; then
            echo -n " -${BEHIND}"
        fi
        if [ "${BEHIND}" -gt 0 ] || [ "${AHEAD}" -gt 0 ] ; then
            echo -n ' )'
        fi
    fi
else
    ansi Color.RED
    echo -n "NO REMOTE"
    ansi Color.DEFAULT
fi
echo -n "]"

#!/bin/bash
set -xe
if [ -z "$1" ] ; then
    echo "You must supply a remote repo to act as the template" 1>&2
    exit 1
fi
git ls-remote "$1" HEAD || echo "Could not find specified remote $1"
git commit --allow-empty -m 'Adding template as parent'
git remote add template "$1"
git config remote.template.tagopt --no-tags
git fetch --all
git remote set-head template -a
TEMPLATE_HEAD=$(git rev-parse --abbrev-ref --symbolic-full-name template/HEAD 2>/dev/null)
if [ -z "${TEMPLATE_HEAD}" ] ; then
    echo "Could not find HEAD of template" 1>&2
    exit 1
fi
git replace --graft HEAD $(git show --pretty="format:%P" --no-abbrev-commit --no-patch HEAD) template/HEAD

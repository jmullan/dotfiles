#!/bin/sh
if [ $# -lt 1 ] ; then
    echo "You have to supply the source directory as an argument"
    exit
fi
SRC_DIR=$1"/submodules/travis-build"
if [ ! -e "${SRC_DIR}" ] ; then
    echo "Your source directory argument must refer to a source directory"
    exit
fi
cd "${SRC_DIR}"
gem install travis -v 1.7.2 --user-install --no-rdoc --no-ri
gem build travis-build.gemspec

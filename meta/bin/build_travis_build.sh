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
gem install travis --user-install --no-rdoc --no-ri
bundle install --path ~/.gem/
gem build travis-build.gemspec
rm -f travis-build-0.0.1.gem

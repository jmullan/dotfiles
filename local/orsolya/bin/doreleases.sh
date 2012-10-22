#!/bin/sh

cd /home/jmullan/packaging/gallery3-packaging
./build.php master >/dev/null
mv dist/* $LOCAL/G3/

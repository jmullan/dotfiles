#!/bin/sh
if [ $# -lt 1 ] ; then
    echo "You have to supply the source directory as an argument"
    exit
fi
SRC_DIR=$1"/portable"
if [ ! -e "$SRC_DIR" ] ; then
    echo "Your source directory argument must refer to a source directory"
    exit
fi
for FULLFILE in `find "$SRC_DIR" -mindepth 1 -maxdepth 1 -not -name '.git' | grep -v '~$' ` ; do
    BASENAME=`basename "$FULLFILE"`
    if [ ! -e ~/"$BASENAME" ] ; then
	echo "Linking $FULLFILE to ~/$BASENAME";
	ln -s "$FULLFILE" ~/"$BASENAME";
    else
	ls -lAd ~/"$BASENAME";
    fi
done
SRC_DIR=$1"/local/"`hostname`
if [ -e "$SRC_DIR" ] ; then
    for FULLFILE in `find "$SRC_DIR" -mindepth 1 -maxdepth 1 -not -name '.git' | grep -v '~$' | grep -v '.bak'` ; do
        BASENAME=`basename "$FULLFILE"`
        LOCALFILE=`hostname`"_$BASENAME"
        echo ~/"$LOCALFILE"
        if [ ! -e ~/"$LOCALFILE" ] ; then
	    echo "Linking $LOCALFILE to ~/$LOCALFILE";
	    ln -s "$FULLFILE" ~/"$LOCALFILE";
        else
	    ls -lAd ~/"$LOCALFILE";
        fi
    done
fi
if [ -e ~/Library/Preferences/PyCharm30/colors ]; then
    cp "submodules/pycharm-solarized/Solarized Dark.xml" ~/Library/Preferences/PyCharm30/colors/;
fi
if [ -e ~/Library ]; then
    mkdir -p ~/Library/KeyBindings
    if [ ! -e ~/Library/KeyBindings/DefaultKeyBinding.dict ] ; then
        ln -s local/osx/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/DefaultKeyBinding.dict
    fi
fi

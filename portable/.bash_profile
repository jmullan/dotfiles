# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.


umask 022

if [ -f ~/.bash_profile_local ]; then
    source ~/.bash_profile_local
fi

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

if [ -e /Users/jmullan ] ; then
    export HOME=/Users/jmullan
else
    if [ -e /home/jmullan ] ; then
        export HOME=/home/jmullan
    else
	export HOME=~
    fi
fi

OLD_IFS="$IFS"
POSSIBLE_PATHS="/opt/icon/bin:/usr/local/share/python:/usr/local/bin:/usr/local/icon/bin:/usr/local/sbin:${HOME}/src/hadoop:${HOME}/bin/ec2/bin:/cygdrive/c/Program Files (x86)/CollabNet/Subversion Client:/cygdrive/c/imvu/Reactor/Core/mysql/bin:/cygdrive/c/Program Files/Java/jdk1.6.0_21/bin:/usr/local/mysql/bin/:${HOME}/.local/bin/"
IFS=":"
for p in $POSSIBLE_PATHS; do
    if [ -d "$p" ] ; then
        export PATH="${p}:${PATH}"
    fi
done
IFS="$OLD_IFS"

for _DIR in `find -L $HOME -maxdepth 1 -mindepth 1 -name 'bin-*' -type d` ; do
    export PATH=$_DIR:"${PATH}";
done

if [ -e $HOME/bin ] ; then
    for _DIR in `find -L $HOME/bin -maxdepth 1 -mindepth 1 -type d` ; do
        export PATH=$_DIR:"${PATH}";
    done
    export PATH=$HOME/bin:"${PATH}"
fi

if [ -e $HOME/src ] ; then
    for _DIR in `find -L $HOME/src -maxdepth 1 -mindepth 1 -name 'bin-*' -type d` ; do
        export PATH=$_DIR:"${PATH}";
    done
fi

if [ -e $HOME/lib/python ] ; then
    for _DIR in `find -L $HOME/lib/python -maxdepth 1 -mindepth 1 -type d` ; do
        export PYTHONPATH=$_DIR:"${PATH}";
    done
fi

if [ -e /usr/lib/jvm/java-6-sun ] ; then
    export JAVA_HOME="/usr/lib/jvm/java-6-sun"
fi

if [ -e /usr/libexec/java_home ] ; then
    export JAVA_HOME=`/usr/libexec/java_home`
fi

if [ -e $HOME/src/hadoop ] ; then
    export HADOOP_HOME=$HOME/src/hadoop
fi

if [ -e $HOME/bin/ec2/bin ] ; then
    export EC2_HOME=$HOME/bin/ec2
fi

if [ -e $HOME/.ec2 ] ; then
    if [ -e $HOME/.ec2/pk.pem ] ; then
        export EC2_PRIVATE_KEY="${HOME}/.ec2/pk.pem"
    fi
    if [ -e $HOME/.ec2/cert.pem ] ; then
        export EC2_CERT="${HOME}/.ec2/cert.pem"
    fi
fi

test -r /sw/bin/init.sh && . /sw/bin/init.sh

alias nodeunit=node_modules/nodeunit/bin/nodeunit

if which ruby >/dev/null && which gem >/dev/null; then
    RUBYPATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
    if [ -e "${RUBYPATH}" ] ; then
        export PATH="${RUBYPATH}:${PATH}"
    fi
fi

# OPAM configuration
. /home/jmullan/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

if which pip >/dev/null; then
    mkdir -p "${HOME}/.cache/pip/wheelhouse"
    export STANDARD_CACHE_DIR="${XDG_CACHE_HOME:-${HOME}/.cache}/pip"
    export WHEELHOUSE="${STANDARD_CACHE_DIR}/wheelhouse"
    export PIP_FIND_LINKS="file://${WHEELHOUSE}"
    export PIP_WHEEL_DIR="${WHEELHOUSE}"

    mkdir -p "${HOME}/.cache/pip/packages"
    export PIP_DOWNLOAD_CACHE="${HOME}/.cache/pip/packages"
fi

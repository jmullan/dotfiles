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

if [ -e $HOME/bin ] ; then
    export PATH=$HOME/bin:"${PATH}"
fi

if [ -e $HOME/`hostname`_bin ] ; then
    export PATH=$HOME/`hostname`_bin:"${PATH}"
fi

if [ -e $HOME/bin ] ; then
    export PATH=$HOME/bin:"${PATH}"
fi

if [ -e $HOME/imvubin ] ; then
    export PATH=$HOME/imvubin:"${PATH}";
fi

if [ -e $HOME/tenxerbin ] ; then
    export PATH=$HOME/tenxerbin:"${PATH}";
fi

if [ -e $HOME/ecofactorbin ] ; then
    export PATH=$HOME/ecofactorbin:"${PATH}";
fi

if [ -d /opt/icon/bin ] ; then
    export PATH="${PATH}":/opt/icon/bin;
fi

if [ -d $HOME/tenxer/tenxer/resources/arcanist/bin ] ; then
    export PATH="${PATH}":$HOME/tenxer/tenxer/resources/arcanist/bin;
fi

if [ -d /usr/local/share/python ] ; then
    export PATH=/usr/local/share/python:"${PATH}";
fi

if [ -d /usr/local/bin ] ; then
    export PATH=/usr/local/bin:"${PATH}";
fi

if [ -d /usr/local/icon/bin ] ; then
    export PATH="${PATH}":/usr/local/icon/bin;
fi

if [ -d /usr/local/sbin ] ; then
    export PATH="${PATH}":/usr/local/sbin;
fi

if [ -e /usr/lib/jvm/java-6-sun ] ; then
    export JAVA_HOME="/usr/lib/jvm/java-6-sun"
fi

if [ -e /usr/libexec/java_home ] ; then
    export JAVA_HOME=`/usr/libexec/java_home`
fi

if [ -e $HOME/src/hadoop ] ; then
    export HADOOP_HOME=$HOME/src/hadoop
    export PATH=$PATH:$HADOOP_HOME/bin
fi

if [ -e $HOME/bin/ec2/bin ] ; then
    export EC2_HOME=$HOME/bin/ec2
    export PATH=$PATH:$EC2_HOME/bin
fi

if [ -e $HOME/.ec2 ] ; then
    if [ -e $HOME/.ec2/pk.pem ] ; then
        export EC2_PRIVATE_KEY="${HOME}/.ec2/pk.pem"
    fi
    if [ -e $HOME/.ec2/cert.pem ] ; then
        export EC2_CERT="${HOME}/.ec2/cert.pem"
    fi
fi

if [ -d /cygdrive/c/Program\ Files\ \(x86\)/CollabNet/Subversion\ Client ] ; then
    export PATH=/cygdrive/c/Program\ Files\ \(x86\)/CollabNet/Subversion\ Client/:"${PATH}";
fi

if [ -d /cygdrive/c/imvu/Reactor/Core/mysql/bin ] ; then
    export PATH=/cygdrive/c/imvu/Reactor/Core/mysql/bin/:"${PATH}";
fi

if [ -d /cygdrive/c/Program\ Files/Java/jdk1.6.0_21/bin ] ; then
    export PATH=/cygdrive/c/Program\ Files/Java/jdk1.6.0_21/bin/:"${PATH}";
fi

if [ -d /usr/local/mysql/bin/ ] ; then
    export PATH=/usr/local/mysql/bin/:"${PATH}";
fi
test -r /sw/bin/init.sh && . /sw/bin/init.sh

alias nodeunit=node_modules/nodeunit/bin/nodeunit

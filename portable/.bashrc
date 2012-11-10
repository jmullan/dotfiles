# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
[ -z "$PS1" ] && return

HOST_SYMBOL="SET UP YOUR LOCALRC FREAL"
HOST_COLOR="3"
# If running interactively, then:
if [ -e .ubuntu-bashrc ] ; then
    source .ubuntu-bashrc
fi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

alias screen='TERM=screen screen'

#stty erase '^?'
#bind '"\C-h": backward-delete-char'
#bind '"\C-?": delete-char'
#bind '"\C-d": delete-char'
bind '"\M-[3~": delete-char'
set editing-mode emacs
bind '"\e[3~": delete-char'
# this is actually equivalent to "\C-?": delete-char
# VT
bind '"\e[1~": beginning-of-line'
bind '"\e[4~": end-of-line'
# kvt
bind '"\e[H": beginning-of-line'
bind '"\e[F": end-of-line'
# rxvt and konsole (i.e. the KDE-app...)
bind '"\e[7~": beginning-of-line'
bind '"\e[8~": end-of-line'

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
if [ -n "$YROOT_NAME" ]; then
    export HISTFILE="$HOME/.history_`hostname`_$YROOT_NAME"
else
    export HISTFILE="$HOME/.history_`hostname`"
fi
export HISTSIZE=10000
export HISTFILESIZE=1000000
export HISTTIMEFORMAT='%F %T '
shopt -s histappend

    # enable color support of ls and also add handy aliases
if [ `which dircolors` ] ; then
    eval `dircolors -b`
fi
alias bc='bc -lq .bcrc'
    #alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'

    # some more ls aliases
    #alias ll='ls -l'
    #alias la='ls -A'
    #alias l='ls -CF'
alias gcc='gcc -Wall'



function _git_prompt() {
    local git_status="`git status -unormal 2>&1`"
    if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
        local origin_diff="`git diff --numstat origin/master | awk 'BEGIN {add=0;del=0}; {add = add + $1; del = del + $2;} ; END {if (add || del) printf "+"add" -"del" ="add + del}'`"
        if [[ "$git_status" =~ nothing\ to\ commit ]]; then
            local ansi=2
        elif [[ "$git_status" =~ nothing\ added\ to\ commit\ but\ untracked\ files\ present ]]; then
            local ansi=6
        else
            local ansi=5
        fi
        if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
            #branch="`git describe --all --contains --abbrev=4 HEAD 2> /dev/null || echo HEAD`"
            branch=`git status | head -n 1 | sed 's/.*On branch //'`
        fi
        if [ -n "$branch$origin_diff" ] ; then
            echo -n "[ "
        fi
        if [ -n "$branch" ]; then
            echo -n "$(tput setaf $ansi)$branch$(tput sgr0)"
            if [[ "$git_status" =~ Changes\ to\ be\ committed ]] ; then
                echo -n " $(tput setaf 7)staged$(tput sgr0)"
            fi
            if [[ "$git_status" =~ not\ staged ]] ; then
                echo -n " $(tput setaf 3)unstaged$(tput sgr0)"
            fi
            if [[ "$git_status" =~ ntracked ]] ; then
                echo -n " $(tput setaf 1)untracked$(tput sgr0)"
            fi
        fi
        if [ -n "$origin_diff" ]; then
            echo -n " $origin_diff "
            ~/bin/gitgraph
        fi
        if [ -n "$branch$origin_diff" ] ; then
            echo -n " ]"
        fi
    fi
}

# set a fancy prompt
if [ -n "$YROOT_NAME" ]; then
    export PS1="\e[36m\u@\H[\e[31m$YROOT_NAME\e[36m]:\w\e[0m\n\$ "
else
    export PS1="\[\e[36m\u@$(tput bold)$(tput setaf $HOST_COLOR)\]\h\[$(tput sgr0)\]:\[\w\e[0m\] \$(_git_prompt)\n\$ "
fi

export SVN_EDITOR=emacs
export VISUAL=emacs
export EDITOR=emacs
export TZ='America/Los_Angeles'
export HOSTNAME=`hostname`

if [ ! -s $DISPLAY ]; then
    emacs () {
        /usr/bin/emacs -nw "$@"
    }
    geany () {
        /usr/local/bin/geany "$@" </dev/null >/dev/null 2>/dev/null &
        disown
    }
fi

complete -W "$(echo $((grep -h '^ssh ' ~/.*history* | sed 's/^ssh //' ; grep ^Host ~/.ssh/config | sed 's/^Host //') | sort -u))" ssh

alias emacs='emacs -nw'

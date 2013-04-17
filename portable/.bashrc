# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
[ -z "$PS1" ] && return

HOST_SYMBOL="SET UP YOUR LOCALRC FREAL"
HOST_COLOR="3"
export GIT_EDITOR=`which emacs`
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

# alias screen='TERM=screen screen'

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

export HISTCONTROL=ignoredups
if [ -n "$YROOT_NAME" ]; then
    export HISTFILE="$HOME/.history_`hostname`_$YROOT_NAME"
else
    export HISTFILE="$HOME/.history_`hostname`"
fi
export HISTSIZE=10000
export HISTFILESIZE=1000000
export HISTTIMEFORMAT='%F %T '
export HISTIGNORE='exit'
shopt -s histappend

# enable color support of ls and also add handy aliases

if [ "$TERM" != "dumb" ]; then
    if [[ "$OSTYPE" =~ ^darwin ]]; then
        alias ls='ls -G'
    else
        alias ls='ls --color=auto'
    fi
    if [ `which dircolors` ] ; then
        [ -e "$HOME/.dircolors" ] && DIR_COLORS="$HOME/.dircolors"
        [ -e "$DIR_COLORS" ] || DIR_COLORS=""
        eval `dircolors -b $DIR_COLORS`
    else
        export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'
    fi
fi
alias bc='bc -lq .bcrc'
alias gcc='gcc -Wall'

######################################################################
# Prefer US English and use UTF-8
######################################################################

export LC_ALL="en_US.UTF-8"
export LANG="en_US"




function _git_prompt() {
    local git_status="`git status -unormal 2>&1`"
    if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
        USER_NAME=`git config user.name`
        USER_EMAIL=`git config user.email`
        if [ -z "$USER_NAME" ]; then
            echo
            echo "$(tput setaf 1)No username set for git: git config user.name 'Jesse Mullan'$(tput sgr0)"
        fi
        if [ -z "$USER_EMAIL" ]; then
            echo
            echo "$(tput setaf 1)No email set for git: git config user.email 'jmullan@visi.com'$(tput sgr0)"
        fi

        TRACKING=`git config prompt.tracking`
        if [ -z "$TRACKING" ]; then
            REMOTES=`git remote`
            if [ -z "$REMOTES" ] ; then
                TRACKING='master'
            else
                TRACKING='origin/master'
            fi
        fi
        git_dir="$(git rev-parse --git-dir 2>/dev/null)"
        if [ -f "$git_dir/rebase-merge/interactive" ]; then
            rebase="INTERACTIVE REBASING"
        elif [ -d "$ggit_dir/rebase-merge" ]; then
            rebase="MERGING REBASE"
        else
            if [ -d "$git_dir/rebase-apply" ]; then
                if [ -f "$git_dir/rebase-apply/rebasing" ]; then
                    rebase="REBASING"
                elif [ -f "$git_dir/rebase-apply/applying" ]; then
                    rebase="APPLYING REBASE"
                else
                    rebase="REBASING"
                fi
            fi
        fi
        if [ -f "$git_dir/rebase-merge/head-name" ] ; then
            rebasehead="$(cat "$git_dir/rebase-merge/head-name")"
        fi

        local origin_diff="`git diff --numstat $TRACKING | awk 'BEGIN {add=0;del=0}; {add = add + $1; del = del + $2;} ; END {if (add || del) printf "+"add" -"del" ="add + del}'`"
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
            #branch=$(git name-rev HEAD 2> /dev/null | awk "{ print \$2 }")
        fi
        if [ -n "$branch$origin_diff" ] ; then
            echo -n "["
        fi
        if [ -n "$rebase" ]; then
            echo -n " $rebase "
        fi
        if [ -n "$rebasehead" ]; then
            echo -n " $rebasehead "
        fi
        if [ -n "$branch" ]; then
            echo -n " $(tput setaf $ansi)$branch$(tput sgr0)"
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

if [ -e ~/.node_completion ] ; then
# {{{
# Node Completion - Auto-generated, do not touch.
shopt -s progcomp
for f in $(command ls ~/.node-completion); do
  f="$HOME/.node-completion/$f"
  test -f "$f" && . "$f"
done
# }}}
fi

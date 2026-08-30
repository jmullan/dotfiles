# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

if [ -f ~/.bashrc_local ]; then
    source ~/.bashrc_local
fi

# quit early if there is no prompt?
[ -z "$PS1" ] && return

HOST_SYMBOL="SET UP YOUR LOCALRC FREAL"
HOST_COLOR="3"
export GIT_EDITOR=emacs
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
export HISTFILE="$HOME/.history_$(hostname)"
export HISTSIZE=10000
export HISTFILESIZE=10000000
export HISTTIMEFORMAT='%F %T '
export HISTIGNORE='exit'
shopt -s histappend

# enable color support of ls and also add handy aliases


if [ "$TERM" != "dumb" ]; then
    if [[ "${OSTYPE}" == darwin* ]]; then
        if command -v gls >/dev/null 2>&1; then
            alias ls='gls --color=auto'
        else
            alias ls='ls -G'
        fi
    else
        alias ls='ls --color=auto'
    fi
    if command -v dircolors >/dev/null 2>&1; then
        if [ -e "${HOME}/.dircolors" ]; then
            eval "$(dircolors -b "${HOME}/.dircolors")"
        else
            eval "$(dircolors -b)"
        fi
    else
        LS_COLORS_FILE="${HOME}/.config/jmullan/ls.j_colors"
        if [ -e "${LS_COLORS_FILE}" ] && command -v j-colors.sh >/dev/null 2>&1; then
            export LS_COLORS="$(j-colors.sh "${LS_COLORS_FILE}")"
        fi
    fi

    if [ -z "${LSCOLORS}" ] && command -v ls_colors_to_lscolors.sh >/dev/null 2>&1; then
        export LSCOLORS="$(ls_colors_to_lscolors.sh)"
    fi

    if [ -z "${LSCOLORS}" ]; then
        export LSCOLORS="ExGxFxdaBxDaDaabababab"
    fi

    if [ -z "${GREP_COLORS}" ] ; then
        GREP_COLORS_FILE="${HOME}/.config/jmullan/grep.j_colors"
        if [ -e "${GREP_COLORS_FILE}" ] && command -v j-colors.sh >/dev/null 2>&1; then
            export GREP_COLORS="$(j-colors.sh "${GREP_COLORS_FILE}")"
        fi
    fi
fi
alias bc='bc -lq .bcrc'
alias gcc='gcc -Wall -O3'
if command -v tree 2>/dev/null ; then
    if command tree --help 2>&1 | grep -q -- '--condense'; then
        alias tree='tree -F --dirsfirst --condense --compress 2'
    else
        alias tree='tree -F --dirsfirst'
    fi
fi
######################################################################
# Prefer US English and use UTF-8
######################################################################

export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export LANGUAGE="en"
# set a fancy prompt
function __prompt_command() {
    . ~/bin/jmprompt
}
export -f __prompt_command
export PROMPT_COMMAND=__prompt_command
export SVN_EDITOR=emacs
export VISUAL=emacs
export EDITOR=emacs
export TZ='America/Los_Angeles'
export HOSTNAME=$(hostname)

if [ ! -s $DISPLAY ]; then
    EMACS=`which emacs`
    emacs () {
        "$EMACS" -nw "$@"
    }
    geany () {
        /usr/local/bin/geany "$@" </dev/null >/dev/null 2>/dev/null &
        disown
    }
fi

complete -C _ssh_complete ssh

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

# added by travis gem
[ -f /home/jmullan/.travis/travis.sh ] && source /home/jmullan/.travis/travis.sh

if [ -e "${HOME}/.cargo/env" ] ; then
    . "${HOME}/.cargo/env"
fi

[[ -f ~/.xnoinitrc ]] && exit
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
[[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
xterm -geometry 100x50+100+40 -fn *-fixed-*-*-*-20-* -bg black -fg white &

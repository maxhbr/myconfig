#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# my login manager
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
elif [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty2 ]]; then
    exec tmux # -2
elif [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty6 ]]; then
    exec startx
fi

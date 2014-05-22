# ~/.bash_profile
#
# written by maximilian-huber.de
#
# Last modified: Mo Jan 21, 2013  11:54

[[ -f ~/.zshrc ]] && . ~/.zshrc

# my login manager
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx &> /tmp/xlog1
elif [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty2 ]]; then
    exec tmux # -2
elif [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty6 ]]; then
    exec startx &> /tmp/xlog2
fi

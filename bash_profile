# ~/.bash_profile
#
# written by maximilian-huber.de
#
# Last modified: Sat Jan 19, 2013  10:50PM

[[ -f ~/.bashrc ]] && . ~/.bashrc

# my login manager
if [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx &> /tmp/xlog1
elif [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty2 ]]; then
    exec tmux # -2
elif [[ -z "$DISPLAY" ]] && [[ $(tty) = /dev/tty6 ]]; then
    exec startx &> /tmp/xlog2
fi

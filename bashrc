# ~/.bashrc
#
# written by maximilian-huber.de
#
# Last modified: Sa Aug 10, 2013  07:14

# If not running interactively stop here
[[ $- != *i* ]] && return

[[ -d $HOME/bin ]] && export PATH=$HOME/bin:$PATH

export EDITOR=vim
export VISUAL=$EDITOR
export PAGER=less
export LESS='-iMn'

# aliases
[[ -f ~/.aliasrc ]] && source ~/.aliasrc

# enable programmable completion features
[[ -f /etc/bash_completion ]] && . /etc/bash_completion

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

PS1="\n[ \$(date +%H:%M) \[\e[1;33m\]\u"
[[ "$TERM" != "screen-bce" && "$SSH_CONNECTION" != "" ]] && PS1="$PS1"'@\h'
PS1="$PS1"' \[\e[1;34m\]\w\[\e[m\] '
PS1="$PS1"'$(parse_git_branch)' #git information
PS1="$PS1"']'
[[ -n "$RANGER_LEVEL" ]] && PS1="$PS1"'─[$RANGER_LEVEL]'
PS1="$PS1"'─>  '

case $TERM in
  xterm*|*rxvt*|Eterm|eterm|rxvt-unicode|urxvt)
    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*} ${PWD}"; echo -ne "\007"'  # user@host path
    ;;
  #screen)
    #;;
esac

[[ -n "$TMUX" ]] && export TERM=screen-256color
[[ "$SSH_CONNECTION" != "" ]] && export DISPLAY=:0.0 #spawn on remote display

#history
# ignored if space at the beginning
export HISTIGNORE="&:ls:[bf]g:exit:[ \t]*"
export HISTSIZE=100000
export HISTFILESIZE=50000

shopt -s histappend
# check window size after ervery command
shopt -s checkwinsize
# fix spelling
shopt -s cdspell
# glob dot files
#shopt -s dotglob

#check in local AND home dir
export CDPATH=.:~

#start tmux for every bash:
#if which tmux 2>&1 >/dev/null; then
  ##if not inside a tmux session, and if no session is started, start a new session
  #test -z "$TMUX" && (tmux attach || tmux new-session)
#fi

# HelloText:
echo -e ""; cal -3; echo -e ""

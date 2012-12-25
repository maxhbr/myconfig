# ~/.bashrc
#
# written by maximilian-huber.de
#
# Last modified: Di Dez 25, 2012  06:02

# If not running interactively stop here
[[ $- != *i* ]] && return

[ -d /home/hubi/bin ] && export PATH=/home/hubi/bin:$PATH

export EDITOR="vim"

# aliases
if [ -f ~/.aliasrc ]; then
  source ~/.aliasrc
fi

#start tmux for every bash:
#if which tmux 2>&1 >/dev/null; then
  ##if not inside a tmux session, and if no session is started, start a new session
  #test -z "$TMUX" && (tmux attach || tmux new-session)
#fi

# for tmux: export 256color
[ -n "$TMUX" ] && export TERM=screen-256color
#[ "$TERM" = "linux" ] && export TERM=screen-256color

# enable programmable completion features
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

if [[ "$TERM" != "screen-bce" && "$SSH_CONNECTION" != "" ]]; then
  # remote connection

  export DISPLAY=:0.0 #spawn on remote display
  PS1="  \$(date +%H:%M) \[\e[1;33m\]\u@\h \[\e[1;34m\]\w\[\e[m\]\ $ \[\e[0m\] "
else
  #PS1="┌─[\$(date +%H:%M) \[\e[1;33m\]\u \[\e[1;34m\]\w\[\e[m\]\ ] \n└─>  "
  #PS1="\n [\$(date +%H:%M)] \[\e[1;33m\]\u \e[1;00m\]in \[\e[1;34m\]\w\[\e[m\]\ $ \[\e[0m\] " # has some problems
  PS1="\n[ \$(date +%H:%M) \[\e[1;33m\]\u \[\e[1;34m\]\w\[\e[m\] "
  PS1="$PS1"'$(parse_git_branch)' #git information
  [ -n "$RANGER_LEVEL" ] && PS1="$PS1"'(Rlvl: $RANGER_LEVEL)'
  PS1="$PS1"']─>  '

  case $TERM in
    xterm*|*rxvt*|Eterm|eterm|rxvt-unicode|urxvt)
      export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*} ${PWD}"; echo -ne "\007"'  # user@host path
      ;;
    #screen)
      #;;
  esac
fi

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

# vi key bindings
#set -o vi

# HelloText:
echo -e ""; cal -3; echo -e ""

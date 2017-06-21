if [ $__HOME_ZSHRC_SOURCED ]; then return; fi
__HOME_ZSHRC_SOURCED=1

###############################################################################
# Path to your oh-my-zsh installation.
if [ -z $ZSH ]; then
    export ZSH="$HOME/.oh-my-zsh"
fi

if [ -d $ZSH ]; then
    ZSH_THEME="robbyrussell" # gallifrey
    # DISABLE_AUTO_UPDATE="true"
    # export UPDATE_ZSH_DAYS=13
    ENABLE_CORRECTION="false"
    COMPLETION_WAITING_DOTS="true"
    plugins=(git zsh-syntax-highlighting) # rake ruby

    # Uncomment the following line if you want to disable marking untracked files
    # under VCS as dirty. This makes repository status check for large repositories
    # much, much faster.
    # DISABLE_UNTRACKED_FILES_DIRTY="true"

    source $ZSH/oh-my-zsh.sh
else
    echo 'oh-my-zsh not found'
    echo 'get oh-my-zsh via sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"'
fi
# see: http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
PROMPT='%T ${ret_status}%?%{$reset_color%} $(git_prompt_info)'
RPROMPT='%{$fg[cyan]%}%~%{$reset_color%}'
# RPROMPT=' [%L]'

###############################################################################
# vi key bindings
bindkey -v
export KEYTIMEOUT=1
bindkey '^r' history-incremental-search-backward
# bindkey '^P' up-history
# bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word

###############################################################################

[[ -f ~/.aliasrc ]] && source ~/.aliasrc
[[ -f ~/.aliasrc-private ]] && source ~/.aliasrc-private
[[ -d $HOME/bin ]] && {
  export PATH=$HOME/bin:$PATH
  [[ -d $HOME/bin/stolen ]] && export PATH=$PATH:$HOME/bin/stolen
  [[ -d $HOME/bin/docker ]] && export PATH=$PATH:$HOME/bin/docker
}
[[ -d $HOME/.perl/bin ]] && export PATH=$HOME/.perl/bin:$PATH
# [[ -d $HOME/perl5/bin ]] && export PATH=$HOME/perl5/bin:$PATH
[[ -d $HOME/.cabal/bin ]] && export PATH=$HOME/.cabal/bin:$PATH
[[ -d $HOME/.local/bin ]] && export PATH=$HOME/.local/bin:$PATH
[[ -d $HOME/.gem/ruby/2.3.0/bin ]] && {
    export PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
} || {
    [[ -d $HOME/.gem/ruby/2.2.0/bin ]] && export PATH=$PATH:$HOME/.gem/ruby/2.2.0/bin
}

PATH="$HOME/perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

[[ "$JAVA_HOME" ]] || JAVA_HOME=$(readlink -f $(which java) | sed "s:bin/java::")

###############################################################################
ZDOTDIR=${ZDOTDIR:-${HOME}}
ZSHDDIR="${HOME}/.config/zsh.d"
#HISTFILE="${ZDOTDIR}/.zsh_history"
HISTSIZE=50000
SAVEHIST="${HISTSIZE}"
# export EDITOR="/usr/bin/vim"
export EDITOR="vim"
export VISUAL="vim -p -X"
export TMP="/tmp"
[[ ! -d "${TMP}" ]] && mkdir "${TMP}"
export TEMP="$TMP"
export TMPDIR="$TMP"
export TMPPREFIX="${TMPDIR}/zsh"
## Use a default width of 80 for manpages for more convenient reading
export MANWIDTH=${MANWIDTH:-80}

#Python virtualenvs
export WORKON_HOME=~/workspace/python/virtualenvs

#paralleles compilieren mit haskell
export ncpus=3

###############################################################################
export PIP_REQUIRE_VIRTUALENV=true

###############################################################################
alias -s tex=vim

alias -s pdf=zathura
alias -s ps=zathura
alias -s djvu=zathura

###############################################################################
[[ -f ~/.zshrc.private ]] && source ~/.zshrc.private
[[ -d /nix/store/k1v2g5784sas2fc9fp6flq50fvsck5w7-taskwarrior-2.5.1/share/doc/task/scripts/zsh/ ]] &&
    fpath=(/nix/store/k1v2g5784sas2fc9fp6flq50fvsck5w7-taskwarrior-2.5.1/share/doc/task/scripts/zsh/ $fpath)

###############################################################################
# Start tmux on ssh
if [ "$PS1" != "" -a "${SSH_TTY:-x}" != x ]; then
  if test -z $TMUX && [[ $TERM != "screen" ]]; then
    ( (tmux has-session -t remote && tmux attach-session -t remote) \
      || (tmux -2 new-session -s remote) ) && exit 0
    echo "tmux failed to start"
  else
    export DISPLAY=:0
  fi
fi

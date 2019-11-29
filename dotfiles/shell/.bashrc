#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# alias ls='ls --color=auto'
# PS1='[\u@\h \W]\$ '

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

###############################################################################
export PROMPT_COMMAND='history -a'

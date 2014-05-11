#!/bin/sh
#
#   ~/bin/myTexWrapper.sh
#
#   usage:
#      myTexWrapper.sh (in the correct folder)
#        run latexmk and vim with servername for synctex suport in tmux
#        (tries to guess the latex variant)
#      myTexWrapper.sh lua[latex]
#        run latexmk for lualatex in the setup
#      myTexWrapper.sh xe[latex]
#        run latexmk for xelatex in the setup
#      myTexWrapper.sh line texfile
#        mv cursor in vim session
#      myTexWrapper.sh pdffile line collumn
#        show text in Zathura
#
#   uses: vim, tmux, zathura, latexmk
#
# Written by Maximilian-Huber.de
#
# Last modified: Wed May 07, 2014  08:10

################################################################################
# Aditional Configuration files:
# ~/.latexmkrc
#       $pdflatex = 'pdflatex -interaction=nonstopmode --shell-escape';
#       $dvi_previewer = "start xdvi";
#       $pdf_previewer = "zathura -l error -s -x 'myTexWrapper.sh %{line} \"%{input}\"' %O %S"
# ~/.vimrc
#       function! SyncTexForward()
#         exec 'silent !myTexWrapper.sh % '.line('.')." ".col('.')
#         redraw!
#       endfunction
#       nmap <Leader>f :call SyncTexForward()<CR>
################################################################################

[[ $# < 2 ]] && {
  MAIN=$(grep -l '\\documentclass' *tex 2> /dev/null)
  if [ $? != 0 ]; then
    exit 1
  fi

  SRVR=$(basename "$(dirname "$(realpath "$MAIN")")")

  tmux has-session -t $SRVR 2> /dev/null
  if [ $? != 0 ]; then
    cmd="latexmk"
    ([[ $# == 1 ]] && [[ "$1" == lua* ]] || [[ -d "./lualatexmk" ]]) && {
      cmd="${cmd} -pdflatex=lualatex -outdir=\"lualatexmk\""
    } || {
      ([[ $# == 1 ]] && [[ "$1" == xe* ]] || [[ -d "./xelatexmk" ]]) && {
        cmd="${cmd} -pdflatex=xelatex -outdir=\"xelatexmk\""
      }
    }
    cmd="${cmd} -pdf -synctex=1 -pvc"

    echo "$SRVR" > "$(dirname $MAIN)/.srvr"
    tmux new-session -s $SRVR -n base -d
    tmux send-keys -t $SRVR:1 "${cmd} ${MAIN}" 'C-m'
    tmux split-window -t $SRVR:1
    NEWEST=$(find $DIR -type f -name '*.tex' -printf '%T@ %p\n' \
      | sort -n \
      | tail -1 \
      | cut -f2- -d " ")
    tmux send-keys -t $SRVR:1 "vim --servername ${SRVR} $NEWEST" 'C-m'
    tmux select-layout -t $SRVR:1 main-horizontal
  fi
  tmux attach -t $SRVR
} || {
  [[ -a ".srvr" ]] && {
    SRVR=$(<".srvr")
    DIR="./"
  } || {
    [[ -a "../.srvr" ]] && {
      SRVR=$(<"../.srvr")
      DIR="../"
    } || {
      exit 1
    }
  }
  [[ $# == 2 ]] && {
    # Zathura -> Vim
    vim --servername $SRVR --remote +$1 "$2"
  } || {
    # Vim -> Zathura
    PDF=$(find $DIR -type f -name '*.pdf' -printf '%T@ %p\n' \
      | sort -n \
      | tail -1 \
      | cut -f2- -d " ")
    zathura --synctex-forward "$2:$3:$1" $PDF
    if [ $? != 0 ]; then
      zathura  -l error -s -x 'myTexWrapper.sh %{line} "%{input}"' $PDF&
    fi
  }
}

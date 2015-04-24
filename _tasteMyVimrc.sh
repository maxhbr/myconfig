#!/bin/sh
#
# This script will generate a folder `/tmp/vimtest/` and install my
# vim-environment completely in this folder.
#
# Written by Maximilian-Huber.de
#
# Last Modified: Sat Oct 04, 2014  04:10
if [ ! -d /tmp/vimtest/myconfig ]; then
  # create and cd to test folder
  mkdir -p /tmp/vimtest/.vim && pushd /tmp/vimtest/
  # clone full configuration
  git clone https://github.com/maximilianhuber/myconfig /tmp/vimtest/myconfig
  # copy vimrc
  cp /tmp/vimtest/myconfig/vimrc /tmp/vimtest/.vimrc
  # copy additional vim files
  cp -r /tmp/vimtest/myconfig/vim/* /tmp/vimtest/.vim/

  if [[ 1==1 ]]; then
    # clone plug.vim
    mkdir -p /tmp/vimtest/.vim/autoload
    curl -fLo /tmp/vimtest/.vim/autoload/plug.vim \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    # install all plugins:
    HOME='/tmp/vimtest' vim +PlugInstall +qall
  else
    # clone vundle
    git clone https://github.com/gmarik/Vundle.vim.git \
      /tmp/vimtest/.vim/bundle/Vundle.vim
    # install all plugins:
    HOME='/tmp/vimtest' vim +BundleInstall +qall
  fi
  popd
  alias vimtest="HOME='/tmp/vimtest' vim"
fi
# run vim with my vimrc
HOME='/tmp/vimtest' vim

echo "run this script again, to start vim again"

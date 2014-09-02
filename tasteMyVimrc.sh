#!/bin/sh
#
# This script will generate a folder `/tmp/vimtest/` and install my
# vim-environment completely in this folder.
#
# Written by Maximilian-Huber.de
#
# Last Modified: Tue Sep 02, 2014  07:26
if [ ! -d /tmp/vimtest/myconfig ]; then
  # create and cd to test folder
  mkdir -p /tmp/vimtest/.vim && pushd /tmp/vimtest/
  # clone full configuration
  git clone https://github.com/maximilianhuber/myconfig /tmp/vimtest/myconfig
  # copy vimrc
  cp /tmp/vimtest/myconfig/vimrc /tmp/vimtest/.vimrc
  # copy additional vim files
  cp -r /tmp/vimtest/myconfig/vim/* /tmp/vimtest/.vim/
  # clone vundle
  git clone https://github.com/gmarik/Vundle.vim.git /tmp/vimtest/.vim/bundle/Vundle.vim
  # install all plugins:
  HOME='/tmp/vimtest' vim +BundleInstall +qall
  popd
  alias vimtest="HOME='/tmp/vimtest' vim"
fi
# run vim with my vimrc
HOME='/tmp/vimtest' vim

echo "run this script again, to start vim again"

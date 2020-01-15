#!/bin/sh
# Copyright 2014-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# ~/bin/ncChat.sh
#
# uses ncat (or netcat or nc) for encrypted chatting over LAN
#
# partly from: http://www.nixaid.com/linux/network/encrypted-chat-with-netcat

if [[ "$1" == "-h" ]] ||  [[ "$1" == "--help" ]]; then
  echo "ncChat.sh -h"
  echo "   -h     Displays this message"
  echo "ncChat.sh [-u] [-w width] [-ip serverip] [passwd]"
  echo "   -u     Use the username as prefix"
  echo "   -w     width of output"
  echo "   -ip    Ip of the server, if none is given, a server will be started"
  echo
  echo "* If no passwd is given, it uses a (bad) default password"
  echo "* Order of arguments is important"
  echo "* Needs ncat (or netcat or nc) to be installed"
  echo "* uses openssl for encryption"
  echo
  echo "written by maximilian-huber.de"
  exit 0
fi

###############################################################################
# Global functions / variables
have() { type "$1" &> /dev/null; }
echoDecodedMsg(){
  echo "$bold$(echo "$1" | openssl enc -d -a -A -aes-256-cbc -k ${2})$normal" \
    | fold -sw $width
}

bold=`tput bold``tput setaf 6`
normal=`tput sgr0`

have ncat \
  && { ncat="ncat"; } \
  || { have nc \
    && { ncat="nc"; } \
    || { have netcat  \
      && {ncat="netcat"; } \
      || { exit 1 } ; } ; }

###############################################################################
# Input parameter
inPrefix=""
if [[ "$1" == "-u" ]] ; then
  shift
  inPrefix="$(whoami): "
fi

width=80
if [[ "$1" == "-w" ]] ; then
  shift
  width=$1
  shift
fi

if [[ "$1" == "-ip" ]]; then
  shift
  ncat="${ncat} ${1} 8877"
  shift
else
  ncat="${ncat} -l -p 8877"
fi

pass="DEFAULT_PASSWORD"
if [[ "$#" -eq 1 ]]; then
  pass=$1
fi

###############################################################################
# Main loop
while IFS= read -r userinput;do \
    echo "($(date +'%H:%M')) $inPrefix$userinput" \
      | openssl enc -aes-256-cbc -a -A -k $pass;echo;done \
  | $ncat \
  | while IFS= read -r srvOut;do echoDecodedMsg "$srvOut" "$pass"; done

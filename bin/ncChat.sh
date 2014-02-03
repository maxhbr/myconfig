#!/bin/sh
# ~/bin/ncChat.sh
#
# uses ncat (or netcat or nc) for chatting over LAN
#
# partly from: http://www.nixaid.com/linux/network/encrypted-chat-with-netcat
#
# written by maximilian-huber.de
# Last modified: Mon Feb 03, 2014  10:00

if [[ "$1" == "-h" ]]; then
  echo "ncChat.sh [-h] [-u] [-ip serverip] [passwd]"
  echo "   -h     Displays this message"
  echo "   -u     Use the username"
  echo "   -ip    Ip of the server, if none is given, a server will be started"
  echo "* Order of arguments is important"
  echo "* Needs ncat (or netcat or nc) to be installed"
  exit 0
fi


have() { type "$1" &> /dev/null; }
echoDecodedMsg(){
  echo "$bold$(echo "$1" | openssl enc -d -a -A -aes-256-cbc -k ${2})$normal";
}

bold=`tput bold``tput setaf 6`
normal=`tput sgr0`

if [[ "$1" == "-u" ]]; then
  shift
  inPrefix="$(whoami): "
else
  inPrefix=""
fi

have ncat \
  && { ncat="ncat "; } \
  || { have nc \
    && { ncat="nc "; } \
    || { have netcat  \
      && {ncat="netcat "; } \
      || { exit 1 }
      }
    }

if [[ "$1" == "-ip" ]]; then
  shift
  ncat="${ncat}${1} 8877"
  shift
else
  ncat="${ncat}-l -p 8877"
fi

if [[ "$#" -eq 1 ]]; then
  #encrypted
  pass=$1
  while IFS= read -r userinput;do \
      echo "$inPrefix$userinput" | openssl enc -aes-256-cbc -a -A -k $pass;echo;done \
    | $ncat \
    | while IFS= read -r srvOut;do echoDecodedMsg "$srvOut" "$pass"; done
else
  #not encrypted
  while IFS= read -r userinput; do \
      echo "$inPrefix$userinput" ; done \
    | $ncat \
    | while IFS= read -r srvOut; do echo "$bold$srvOut$normal"; done
fi

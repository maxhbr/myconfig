#!/bin/sh
#
# uses ncat (or netcat or nc) for chatting
#
# partly from: http://www.nixaid.com/linux/network/encrypted-chat-with-netcat
#
# written by maximilian-huber.de
# Last modified: Sun Feb 02, 2014  10:43

have() { type "$1" &> /dev/null; }
echoDecodedMsg(){
  echo "$bold$(echo "$1" | openssl enc -d -a -A -aes-256-cbc -k ${2})$normal"; \
}

bold=`tput bold``tput setaf 4`
normal=`tput sgr0`

#inPrefix="$(whoami): "
inPrefix=""

have ncat \
  && { ncat="ncat "; } \
  || { have nc \
    && { ncat="nc "; } \
    || { have netcat  \
      && {ncat="netcat "; } \
      || { exit 1 }
      }
    }

if [[ "$1" == "-h" ]]; then
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

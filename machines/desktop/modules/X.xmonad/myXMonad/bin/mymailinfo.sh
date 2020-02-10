#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
###############################################################################
## Mailstatus
if ! test `find /tmp/mailrun-sh-log -mmin -2`; then
  echo -n " <fc=#ff0000>¬IMAP</fc> "
fi

###############################################################################
## Mail
# Mails1=$(find "$HOME/Mail/mail/INBOX/new/" -type f | wc -l)
# Mails2=$(find "$HOME/Mail/by-hubi/INBOX/new/" -type f | wc -l)
# Mails3=$(find "$HOME/Mail/alfa/INBOX/new/" -type f | wc -l)
# if test `find /tmp/mailrun-sh-log -mmin -2`; then
#   if ! [[ $Mails2 == "0" && $Mails2 == "0" && $Mails3 == "0" ]]; then
#     echo -n "| <fc=#00ff00>Mail: ${Mails1}/${Mails2}/${Mails3}</fc> "
#   fi
# elif ! [[ $Mails1 == "0" && $Mails2 == "0" && $Mails3 == "0" ]]; then
#   echo -n "| <fc=#ff0000>¬IMAP: ${Mails1}/${Mails2}/${Mails3}</fc> "
# else
#   echo -n "| <fc=#ff0000>¬IMAP</fc> "
# fi

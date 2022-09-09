#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

have() { type "$1" &> /dev/null; }

have gpg && gpg $@ || {
       have gpg2 && gpg2 $@
    }

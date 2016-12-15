#!/usr/bin/env bash

have() { type "$1" &> /dev/null; }

have gpg && gpg $@ || {
       have gpg2 && gpg2 $@ 
    }

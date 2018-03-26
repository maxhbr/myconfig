#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

git filter-branch --prune-empty --index-filter 'git rm -rf --cached --ignore-unmatch $1' --tag-name-filter cat -- --all

git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d
git reflog expire --expire=now --all
git gc --prune=now

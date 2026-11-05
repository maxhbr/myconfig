# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Tab completion for `mysbx-once` (bd myconfig-7zwt, cli.md D22): the
# second name of the mysbx binary, one sandbox run out of a private
# temp root. The verb set is the once subset — the persistent forms
# are REFUSED with exit 70, so offering them would be noise: the bare
# form, `run`, `gui`, `doctor`, `version`, `help`, and the flags that
# apply. Mirrors the structure of mysbx.fish (same `__mysbx_no_verb`
# discipline: a verb ends flag offering) but is deliberately NOT
# sourced from it — the refused verbs differ, and the sync contract of
# the `mysbx-completions` check pins this file to usage.txt's
# `mysbx-once` line on its own.

function __mysbx_once_no_verb
    not __fish_seen_subcommand_from run gui doctor version help
end

complete -c mysbx-once -f -n '__mysbx_once_no_verb' -a run -d 'Run one command in the sandbox: mysbx-once run [flags] -- CMD...'
complete -c mysbx-once -f -n '__mysbx_once_no_verb' -a gui -d 'Run mysbx-once in a new terminal window (needs a graphical session)'
complete -c mysbx-once -f -n '__mysbx_once_no_verb' -a doctor -d 'Check the user-layer configuration (the host sidecar is ignored)'
complete -c mysbx-once -f -n '__mysbx_once_no_verb' -a version -d 'Print the version'
complete -c mysbx-once -f -n '__mysbx_once_no_verb' -a help -d 'Print this help'

# The flags the once forms accept (usage.txt "The same binary, second
# name"): --dry-run, --verbose, --backend, --multiplexer, --ro, --rw
# global; the same set minus --session/--result/--timeout after `run`.
complete -c mysbx-once -l dry-run -d 'Print the argv instead of running (creates nothing)'
complete -c mysbx-once -l verbose -d 'Print the run configuration report'
complete -c mysbx-once -l backend -x -d 'Override the backend for this run'
complete -c mysbx-once -l multiplexer -x -d 'Override the multiplexer for this run'
complete -c mysbx-once -l ro -r -d 'Bind a host path read-only into the sandbox'
complete -c mysbx-once -l rw -r -d 'Bind a host path read-write into the sandbox'

---
name: rtk
description: Use rtk (Rust Token Killer) to keep command output small. Use when running noisy shell commands (ls, cat, grep, git, test runners, docker) in a repository, or when the user asks about rtk, token savings, `rtk gain`, or why a command was rewritten.
---

# rtk — token-efficient command output

`rtk` is a CLI proxy that filters and compresses the output of common dev
commands before it reaches the model context. It is installed on this machine
and is a drop-in prefix: `rtk <command> [args]` runs `<command>` and prints a
compacted version of its output.

## When to prefix a command with `rtk`

Prefix any command whose raw output is long and mostly boilerplate:

```bash
rtk ls                  # tree with file counts instead of one line per entry
rtk cat <file>          # structure/signatures instead of the full body
rtk grep <pattern>      # long lines truncated, matches grouped per file
rtk git status          # compact, grouped by state
rtk git diff            # reduced context
rtk git log             # hash, author, subject only
rtk cargo test          # failures only, passing tests collapsed to a count
rtk pytest              # failures only, traceback trimmed
rtk go test ./...       # failures only
rtk docker ps           # essential fields only
```

Do **not** prefix a command when the exact, complete output matters — for
example when reproducing an error verbatim, or when a subsequent command
consumes the output. Use `rtk proxy <cmd>` to run a command through rtk
without any filtering.

Some harnesses on this host rewrite bash tool calls to `rtk …` automatically
(a `PreToolUse` hook or plugin). Writing `rtk` yourself is still correct — the
rewrite is idempotent.

## Meta commands (always run these as `rtk …`)

```bash
rtk gain                # token savings dashboard
rtk gain --history      # per-command history with savings
rtk discover            # find missed rtk opportunities in past sessions
rtk proxy <cmd>         # run <cmd> unfiltered (debugging)
rtk --version           # verify the installed binary
```

## Configuration

The config file is `~/.config/rtk/config.toml`. On this machine it is
generated declaratively by NixOS/home-manager
(`modules/myconfig.ai/programs.rtk/`) and is therefore **read-only** — do not
edit it, and never run `rtk init`: the agent hooks and plugins it would write
are already deployed as read-only files. Change
`myconfig.ai.rtk.settings` in the NixOS configuration instead and rebuild.

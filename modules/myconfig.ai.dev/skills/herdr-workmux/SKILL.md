---
name: herdr-workmux
description: Spawn parallel fix agents in git worktrees from inside a Herdr pane when the user asks for workmux or a worktree per task. Use when HERDR_ENV=1 and workmux cannot open tmux windows. Do not use outside Herdr, and do not use for a single in-pane edit.
---

# Herdr workmux

Use this when the user asks for workmux, or for one worktree per task, and this agent is already inside Herdr. You are the dispatcher. Do not implement the tasks yourself.

`workmux` is installed and its config starts `pi`, but it places agents in tmux windows. A Herdr pane is not a tmux session. `workmux add` fails here. Herdr creates the worktrees and the agent panes instead. Keep the workmux prompt rules. Drop the workmux window commands.

## Check the session

```bash
test "${HERDR_ENV:-}" = 1
```

If that fails, say you are not inside Herdr and stop. Do not call `herdr`.

Do not start tmux. Do not pass `--parent-session`. Do not run `herdr server stop`.

## What fails, and what replaces it

From the Herdr pane, this is expected:

```text
workmux add <branch> --name <handle> -b -P <prompt-file>
Error: tmux is not running.
```

`workmux add --dry-run` can still succeed. Ignore the dry-run path (`<repo>__worktrees/<handle>`). The live checkout is the path Herdr returns.

Replacement:

| workmux | Herdr |
| --- | --- |
| `workmux add <branch> --name <handle> -b` | `herdr worktree create --cwd "$PWD" --branch <branch> --base main --label <handle> --no-focus` |
| agent pane in that worktree | `herdr agent start <name> --kind pi --pane <root-pane-id>` |
| `-P <prompt-file>` | `herdr agent prompt <name> "$(cat <prompt-file>)"` |
| `workmux status` | `herdr agent get <name>` |
| `workmux capture` | `herdr agent read <name> --source recent-unwrapped --lines 80` |
| `workmux remove` | only if the user asks: `herdr worktree remove --workspace <id>` |

Read every id from the JSON result. Do not invent pane or workspace ids.

- Worktree create returns `.result.worktree.path`, `.result.workspace.workspace_id`, and `.result.root_pane.pane_id`.
- `agent start` returns `.result.agent.name` and `.result.agent.pane_id`. A successful start means the agent is idle and ready for input.

Create every worktree before starting agents. Start every agent before sending prompts.

```bash
herdr worktree create --cwd "$PWD" --branch octrc-ps4 --base main --label octrc-ps4 --no-focus
herdr worktree create --cwd "$PWD" --branch octrc-2sa --base main --label octrc-2sa --no-focus
herdr agent start ps4-fix --kind pi --pane <ps4-root-pane-id>
herdr agent start csv-spec --kind pi --pane <2sa-root-pane-id>
```

`--no-focus` keeps the user's focus in the calling pane. Do not add `--trust-repository` unless the user has already verified the repository.

Agent names must match `[a-z][a-z0-9_-]{0,31}` and be unique among live agents. Check with `herdr agent list` before start. Branch names can match the bead id (`octrc-ps4`). Agent names cannot contain digits first, so use a word name such as `ps4-fix`, not `octrc-ps4`.

## Prompt files

Write every prompt file before any `worktree create`. Use a temp file. The prompt must stand alone. The worker cannot see this conversation.

Include:

- the bead id and `bd update <id> --claim`
- the bug text, expected result, and acceptance criteria from `bd show`
- "implement on this branch; do not merge to main"
- commit on that branch only if the user asked to commit or the bead workflow you were given says to commit
- close the bead only when acceptance is met: `bd close <id> --reason="..."`
- a required report: root cause, files changed, how it was verified, commit hash if any

Send the file contents with `herdr agent prompt`. A prompt result that still says `idle` is not proof of failure. Wait a few seconds, then `herdr agent get <name>`. Continue only when status is `working`. If it stays `idle`, read the pane before sending the prompt again.

```bash
herdr agent prompt ps4-fix "$(cat "$prompt_file")"
sleep 4
herdr agent get ps4-fix
```

Do not use `agent prompt --wait` for a long fix. A timeout does not mean the prompt was lost.

## While they run

Track each handle until you have reported it or the user tells you to remove it.

```bash
herdr agent get ps4-fix
herdr agent get csv-spec
```

`working` means leave it alone. `idle` or `done` means read the result and report it. `blocked` means read the pane and ask the user before sending keys. Do not answer an approval dialog yourself.

Do not merge to `main` unless the user asks. Do not close a Herdr workspace, tab, or pane you created unless the user asks.

## Report

Tell the user that workmux could not open tmux windows, and that Herdr worktrees were used. For each task give the branch, workspace id, agent name, checkout path, and whether the agent is working. Say that nothing was merged to `main`.

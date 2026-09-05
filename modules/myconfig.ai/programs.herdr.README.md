# `herdr` worktrees: the workmux sibling layout

Owner module: [`programs.herdr.nix`](./programs.herdr.nix).

## Layout

`prefix+shift+g` ("new worktree") creates the checkout next to the original
repository, exactly where `workmux` and `git branch-to-worktree`
([`../shell.git/bin/git-branch-to-worktree.sh`](../shell.git/bin/git-branch-to-worktree.sh))
put theirs:

```
<parent-of-repo>/<repo-name>__worktrees/<handle>
```

`<handle>` is the branch name slugified the same way workmux does (lowercased,
every run of non-alphanumeric characters replaced by a single `-`, no leading
or trailing `-`), e.g. `feature/foo bar` -> `feature-foo-bar`.

Example: for the repo `~/myconfig/myconfig`, branch `herdr-worktree-location`
lands in `~/myconfig/myconfig__worktrees/herdr-worktree-location`.

It can be triggered from anywhere in the repository, including from inside a
linked worktree, because it always resolves the **main** checkout first. That
also fixes the base of a *new* branch to the main checkout's `HEAD` — it does
not fork from the worktree the popup was opened in. Use
`git branch-to-worktree` when you want to fork from the current HEAD.

## Why a custom command and not just a setting

herdr has exactly one worktree option, `[worktrees] directory` (default
`~/.herdr/worktrees`). It is a **single global root**, expanded once when the
server starts, under which herdr creates `<repo-name>/<branch-slug>`
checkouts. It has no placeholders and no per-repository resolution, so the
sibling layout cannot be written down as a value of that option.

What herdr *does* support:

- `herdr worktree create --path <ABSOLUTE PATH>` accepts an arbitrary checkout
  path and still registers the result as a herdr-managed *linked worktree*
  workspace, so `open_worktree` / `remove_worktree` keep working on it.
- Custom command keybindings (`[[keys.command]]`) can run an arbitrary command
  and receive `HERDR_ACTIVE_PANE_CWD`.

The module therefore:

1. unbinds the built-in action (`keys.new_worktree = ""`),
2. binds `prefix+shift+g` to a popup running `herdr-worktree-sibling`
   (a `writeShellApplication` in `programs.herdr.nix`, also on `PATH`), which
   resolves the **main** working tree of the focused pane's repository,
   computes the workmux path, and calls `herdr worktree create --path ...`.

Running it from inside a linked worktree works: the script resolves the main
working tree first, because herdr refuses `--cwd` pointing into a linked
worktree ("worktree actions start from the repo parent workspace").

## Remaining gap on the host

Flows that do **not** pass an explicit `--path` — for example a bare
`herdr worktree create` issued by an agent following the herdr skill, or the
socket API — still use the global `[worktrees] directory`
(`~/.herdr/worktrees/<repo>/<branch-slug>`). That is a herdr limitation, not a
configuration choice here; see
[`../../doc/TODOs/herdr-per-repo-worktree-directory.md`](../../doc/TODOs/herdr-per-repo-worktree-directory.md).

## `agent-bubblewrap-herdr`: the sandbox closes that gap

A jail session has exactly **one** repository, so there the global option can
be made repository-local. `agent-bubblewrap-herdr` (same module) is the
bubblewrap analogue of `agent-qemu-herdr` and the workmux jail
([`myconfig.ai.workmux/jail.nix`](./myconfig.ai.workmux/jail.nix)):

1. The wrapper resolves the repository root from the working directory,
   **creates** `<parent-of-repo>/<repo>__worktrees` and binds it read-write
   into the jail *at the identical path* — identical, because `git worktree`
   stores absolute paths in `.git/worktrees/<n>/gitdir`, so a remapped path
   would produce checkouts that are broken on the host.
2. The entrypoint writes a session `~/.config/herdr/config.toml` into the
   jail's tmpfs `$HOME`: the shared keybindings plus
   `[worktrees] directory = "<that sibling directory>"`. The host
   `~/.config/herdr` is deliberately not bound.
3. Because the root is now correct for *every* flow, the jail config restores
   herdr's **built-in** `prefix+shift+g` (plus `prefix+shift+o` /
   `prefix+shift+x` for open/remove) instead of the host's popup command; no
   socket round-trip is involved.
4. herdr runs as `herdr --no-session` (monolithic): no server/client split, so
   a session can never attach to a differently-configured server, and it dies
   with the jail.

herdr still appends `<repo-name>/<branch-slug>` to its root, so inside the
sandbox checkouts land one level deeper than on the host:

```
<parent-of-repo>/<repo>__worktrees/<repo>/<branch-slug>
```

That is intentional and accepted — it is inside the same sibling directory and
usable from the host.

### Working directory of the session

The session comes up in the directory the wrapper was **invoked from**, not in
`$HOME`. Two things are needed for that:

* `bwrap` keeps the host working directory, and `mount-cwd` binds it
  read-write at the identical path, so the invocation directory exists inside
  the jail (the enclosing repository and its `__worktrees` sibling are bound
  too).
* herdr's **first** workspace is nevertheless always rooted at `$HOME` — the
  `[terminal] new_cwd` policy only applies to workspaces created *later*, and
  there is no CLI flag for the initial one. The entrypoint therefore waits for
  the socket API (which `--no-session` serves too), creates a workspace with
  `herdr workspace create --cwd <invocation directory> --focus`, and closes the
  `~` workspace that herdr made itself. If the API never answers, herdr is left
  alone rather than ending up with two workspaces.

`new_cwd = "follow"` is deliberately kept instead of `"current"`: new
tabs/panes must inherit the *focused* workspace's directory, which for a
worktree workspace is the worktree and not the invocation directory.

The jail config also sets `onboarding = false`. The jail's `$HOME` is a tmpfs,
so herdr would otherwise show its first-run wizard on every start and come up
with **no** workspace at all ("No workspaces yet") — which is what made every
manually created workspace land in `$HOME`.

### Where it can be started from

* **Main checkout** — the normal case: the repository and its `__worktrees`
  sibling are bound read-write.
* **Linked worktree** — supported. `<worktree>/.git` is a *file* pointing at
  `<main-repo>/.git/worktrees/<name>`, so the wrapper resolves the shared git
  directory (via that directory's `commondir` file) and binds it read-write as
  well; without it every git command inside the jail fails with "not a git
  repository". The main **checkout** stays invisible — only its git metadata is
  exposed — and the `__worktrees` sibling is computed from the main repository,
  so it is the same directory no matter which checkout the jail was started
  from. Note that read-write access to the shared git directory also means
  write access to the main repository's refs, objects and hooks.
* **Outside a git repository** — herdr starts normally in that directory; only
  `[worktrees]` is left unset, so the worktree actions fall back to herdr's
  default `~/.herdr/worktrees`, which is the jail's tmpfs and therefore
  ephemeral. The entrypoint prints a note about that before starting.
* **`$HOME` itself** — refused. The wrapper's shared guard rejects
  `$PWD == $HOME`, and a second guard rejects the case where the walk up to the
  repository root lands on the home directory (or a parent of it), which would
  otherwise bind all of `$HOME` read-write.

The panes run the plain (un-jailed) agent binaries with their real home state
bound read-write — the jail itself is the sandbox, so no nested sandboxing.

Related: the plain `agent-bubblewrap-pi` wrapper now also `mkdir -p`s the
sibling directory before binding it
([`programs.pi-coding-agent/default.nix`](./programs.pi-coding-agent/default.nix),
`worktreesSiblingPerm`); bubblewrap can only bind a path that already exists,
so without it the first worktree in a repository could not be created.

### `agent-bubblewrap-alacritty-herdr`: the same jail in its own window

`agent-bubblewrap-alacritty-herdr` is the GUI launcher for exactly the jail
described above: it opens an Alacritty window whose command is
`agent-bubblewrap-herdr`, with `--working-directory` set to the directory it
was invoked from (which is what the jail binds read-write and what herdr's
first workspace is relocated to). It is the herdr analogue of
`agent-bubblewrap-alacritty-workmux-tmux`
([`myconfig.ai.workmux/jail.nix`](./myconfig.ai.workmux/jail.nix)).

* It detaches from the calling shell via
  [`lib/detached-gui-launcher.nix`](../../lib/detached-gui-launcher.nix), so
  the terminal is free again immediately (`--foreground` or
  `MYCONFIG_GUI_LAUNCHER_FOREGROUND=1` keeps the old blocking behaviour).
* The `$PWD == $HOME` refusal and the "not a git repository" note are printed
  in the *calling* terminal, before the window opens — inside the window they
  would disappear together with it.
* Unlike the workmux launcher it accepts being started from a **linked
  worktree**, because the herdr jail supports that (see "Where it can be
  started from" above).
* Extra arguments are passed through to `herdr` inside the jail.

## Applying changes

`~/.config/herdr/config.toml` is generated by Nix. After a rebuild, run
`herdr server reload-config` (or restart herdr) to pick the new config up.

# Workspace layout: `central` vs `beside-repo`

Status: implemented.
Owner: `modules/myconfig.ai/myconfig.ai.microvm/workspace.nix` (paths),
`launcher.nix` (behaviour), `default.nix` (options).

## Problem

`agent-microvm run --repository /home/mhuber/myconfig/myconfig --name playwright-cli`
creates the task's standalone clone at

```
/var/lib/agent-microvms/workspaces/myconfig__agent-microvm/playwright-cli
```

The clone is the thing a human must diff, fetch from and merge back, so a
system path under `/var/lib` is the wrong place for it ergonomically: it is not
next to the repository it belongs to, it is not next to the sibling
`myconfig__worktrees/` directory workmux creates, and reaching it invites
`sudo git`, which writes root-owned files into a uid-1000 clone.

The expectation is that a task clone of `/home/mhuber/myconfig/myconfig` lands
at

```
/home/mhuber/myconfig/myconfig__agent-microvm/playwright-cli
```

i.e. beside the source repository, mirroring the `<project>__worktrees`
convention of workmux.

## Why the central root existed

Three properties depended on "all clones live under one root":

1. **Task-addressed commands could enumerate.** `workspace-remove <task>`,
   `usage` and `dashboard` globbed `<workspaceRoot>/*/<task>` and
   `<workspaceRoot>/*/*`. A task name alone is not a path, and with clones
   scattered beside arbitrary repositories there is nothing to scan.
2. **`validate_repository` used the location as a guard.** It refused a
   `--repository` that is itself an agent workspace by testing whether the git
   toplevel sits under `<workspaceRoot>`. That test silently stops matching for
   clones that live elsewhere.
3. **Mode/ownership predictability.** The clone is bind-mounted into an
   untrusted guest and chowned to uid/gid 1000. Under `/var/lib` its parents
   are root-owned 0755; under `$HOME` they are whatever the user has.

None of the three requires a *central* root — they require a *registry*, an
*intrinsic* repository-is-a-workspace test, and *explicit* mode handling.

## Design

### 1. `workspaceLayout` selects where the clone group is created

New option `myconfig.ai.microvm.workspaceLayout`, an enum:

| value | clone path |
| --- | --- |
| `central` (default) | `<workspaceRoot>/<repoSlug>__agent-microvm/<task>` |
| `beside-repo` | `<dirname repoToplevel>/<repoName>__agent-microvm/<task>` |

The *group directory name* is identical in both layouts: it is
`repo_slug()`'s output, which already appends the literal `__agent-microvm`
suffix. Only the parent differs, so `clone_path()` stays the single place that
computes the shape.

`beside-repo` refuses a repository whose parent directory is `/`, or which sits
inside `runtimeRoot`, `stateRoot` or `workspaceRoot` — a guest-writable tree
must never be created inside those.

### 2. A registry replaces the glob

`<runtimeRoot>/workspace-index/<task>` is a root-owned symlink to the task's
clone, written by `create_clone` and removed by `workspace-remove`. It is the
authoritative task → clone mapping for every task-addressed command:

* `workspace-remove <task>` resolves the clone through it,
* `usage` / `dashboard` enumerate it,
* `recover` prunes dangling entries,
* `validate_repository` refuses a `--repository` that is a registered clone.

The index directory is root-owned 0755 and lives in `runtimeRoot`, which no
guest ever sees, so the agent cannot retarget an entry.

For backwards compatibility, enumeration is the **union** of the index and the
legacy `<workspaceRoot>/*/*` glob, so clones created before this change stay
visible to `usage` and removable by `workspace-remove`.

### 3. Removal is guarded by intrinsic properties, not by location

`rm -rf` on a path obtained by following a symlink needs the target to be
proven, not assumed. `workspace-remove` refuses unless the resolved path

* is a directory whose basename equals the task name,
* sits in a group directory whose name ends in `__agent-microvm`,
* contains `.git`,
* is owned by the guest agent uid,
* is not `runtimeRoot`/`stateRoot` itself or `/`.

This holds for both layouts and does not depend on the clone being under
`workspaceRoot`.

### 4. `validate_repository` gains an intrinsic test

In addition to the `workspaceRoot` subtree test (kept for the central layout),
a repository is refused when

* its parent directory name ends in `__agent-microvm`, or
* it is registered in the workspace index.

The `__agent-microvm` suffix is therefore **reserved**: a directory of that
name is treated as an agent workspace group in every layout.

### 5. Reporting

`usage`/`dashboard` print the clone's full path in the table's `REPO` column
context. Under `beside-repo` the `workspaces:` footer reports the summed size
of the enumerated clones instead of the size of `workspaceRoot`, which is empty
in that layout.

## Consequences

* `workspaceRoot` is only meaningful for `central` (and for legacy clones).
  It stays a first-class option because the index root is deliberately *not*
  the storage root.
* A `beside-repo` clone is created by root inside a user-owned directory. The
  group directory is chowned to the guest agent uid (1000) on creation so the
  human owner can list, fetch from and delete it without `sudo`.
* `myconfig.ai.microvm` hosts that switch layout keep their old clones; they
  are still enumerated and removable, but new tasks land in the new location.

## Verification

* `nix build .#checks.x86_64-linux.microvm-batch-launcher-submit` — executes the
  real launcher (bwrap + fakeroot) under the **central** layout.
* `nix build .#checks.x86_64-linux.microvm-batch-launcher-submit-beside-repo` —
  the same harness against a launcher built with
  `workspaceLayout = "beside-repo"`, asserting the clone lands beside the
  source repository, that the index entry exists, and that `workspace-remove`
  deletes both.
* `nix build .#checks.x86_64-linux.microvm-eval-workspace-layout` — eval-level
  proof that the option changes the generated launcher and the tmpfiles rules.

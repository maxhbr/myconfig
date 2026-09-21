# Patch switch-to-configuration-ng to skip users without a session bus

Upstream bug: `switch-to-configuration-ng`
(`pkgs/by-name/sw/switch-to-configuration-ng/src/main.rs`, "Reload user units"
loop) decides whether to run per-user activation by stat()ing
`/run/user/<uid>`, assuming the directory exists iff `user@<uid>.service`
is active (BindsTo=). That assumption breaks for:

- the greetd `greeter` user (uid 990): the runtime dir lingers from an
  earlier greetd session while no user manager / session bus runs;
- linger users (`users.users.<name>.linger = true`, e.g. `agent` uid 31000
  from `modules/myconfig.agentUsers.nix`) whose
  `user-runtime-dir@<uid>.service` is up but `user@<uid>.service` (and
  its dbus-broker) is not.

The per-user child then fails with
`Failed to connect to socket /run/user/<uid>/bus: Connection refused` and the
whole `nixos-rebuild switch` exits 4 even though nothing that matters failed
(no such user has user units worth reloading).

The local patch probes the session bus socket (`/run/user/<uid>/bus`) before
spawning the per-user child and skips users without one, matching the intent
of the upstream comment. Ideally this becomes an upstream fix — the issue has
NOT yet been filed (no GitHub credentials available while writing this);
file it against NixOS/nixpkgs with the reproduction notes above and record
the link here:
  <link-to-nixpkgs-issue>

## What to remove once upstream is fixed

- `modules/system.switch/skip-users-without-session-bus.patch`
- the overlay that applies it: `modules/system.switch/overlay.nix`
  (then `modules/system.switch/default.nix` only sets an empty
  `nixpkgs.overlays` list — simplify or drop the whole
  `modules/system.switch/` directory if nothing else lands there).

## How to verify upstream is fixed

- The pinned nixpkgs `pkgs/by-name/sw/switch-to-configuration-ng/src/main.rs`
  no longer relies solely on the `/run/user/<uid>` stat in the
  "Reload user units" loop, or probes the session bus / user manager state
  before spawning the per-user child.
- A `nixos-rebuild switch` on `workstation` with `user@31000.service`
  inactive and `/run/user/31000` present exits 0 and prints
  `skipping user agent: ... (user manager not running)` (or equivalent)
  instead of `warning: user activation for agent failed`.

## Reference

Introduced by the "Stop switch-to-configuration exiting 4 on stale
/run/user/<uid>" task (worktree `stale-runtime-dir-user-activation`); see
`git log modules/system.switch/` for the commit.

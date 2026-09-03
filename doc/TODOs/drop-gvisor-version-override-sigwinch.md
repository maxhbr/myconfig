# Drop the gVisor version override once nixpkgs ships gVisor ≥ 2026-06-05

## What to remove

The `version` / `src` / `vendorHash` / `patches = [ ]` override in
`modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/nix/overlay.nix`
pins gVisor `20260817.0` (`b1b561450fc2f05b9626b7e269c08fbc9f5029ff`,
"Merge release-20260817.0-38-ged1b001b8 (automated)" on the synthetic
`go` branch) ahead of nixpkgs, which still ships `20260406.0`
(`db8d2c9abca39156c61ee2769d52b8a11accbe16`, from
`pkgs/by-name/gv/gvisor/package.nix`).

When removing the override:

- Keep the `gvisor-remove-p2p-addresses.patch` patch (it is not upstream,
  see `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/docs/debug-runsc-tun0-netns.md`
  and `doc/TODOs/` — it must continue to apply; verify hunks still match).
- Drop the `patches = [ ];` reset only if nixpkgs' `fix-go-mod-tidy.diff`
  still applies to whatever gVisor revision nixpkgs ships at that point
  (it did NOT apply to 20260817.0, which is why the reset exists).

## Why the override exists

The nixpkgs pin (20260406.0) predates upstream gVisor commit
`cfb7c0629521099eb14d7bd86e9fbfa47287a640` (2026-06-05, "Fix SIGWINCH
delivery on PTY window size change (TIOCSWINSZ)", fixes
google/gvisor#13317). Without it, `lineDiscipline.setWindowSize`
(`pkg/sentry/fsimpl/devpts/line_discipline.go`) only stores the new
window size and never sends `SIGWINCH` to the pty's foreground process
group. Consequence inside `agent-gvisor` sandboxes: when the host
terminal (foot) is resized, herdr resizes each pane pty correctly
(`TIOCGWINSZ` returns the new size), but the program in the pane
(pi, shells, editors) never gets `SIGWINCH` and keeps rendering at the
old width.

## Condition for removal

nixpkgs ships a gVisor release containing `cfb7c0629521099eb14d7bd86e9fbfa47287a640`,
i.e. any release ≥ 2026-06-05 (e.g. `20260608.0`, `20260817.0`). Check
`pkgs/by-name/gv/gvisor/package.nix` for `version`/`rev`.

## How to verify

1. `nix eval` the host's gVisor version and confirm
   `grep -c SIGWINCH` in `pkg/sentry/fsimpl/devpts/line_discipline.go`
   of the pinned source (the upstream fix adds exactly one).
2. Rebuild and restart the sandbox, then inside a herdr pane run the
   in-guest reproduction from
   `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/docs/` (or a
   simple `python3` openpty test): `TIOCSWINSZ` on the master must
   deliver `SIGWINCH` to the foreground process group.
3. End-to-end: resize the host foot window and confirm the TUI inside
   the herdr pane re-renders at the new width.

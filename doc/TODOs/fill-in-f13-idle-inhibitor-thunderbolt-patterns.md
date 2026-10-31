# Fill in f13's waybar idle inhibitor Thunderbolt patterns

`hosts/host.f13/default.nix` should set
`myconfig.desktop.wayland.waybar.idleInhibitor.thunderboltDevices`
(the option is defined in
`modules/myconfig.desktop.wayland/programs.waybar/idleInhibitor.nix`)
so docking into the TB station keeps the screen unlocked and undocking
re-enables idle locking.

Steps:

1. Dock the f13 TB station.
2. Collect patterns:
   `cat /sys/bus/thunderbolt/devices/*/device_name` (also `vendor_name`,
   `unique_id`) — e.g. `"Caldigit TS4"`.
3. Set the option in `hosts/host.f13/default.nix`:

   ```nix
   myconfig.desktop.wayland.waybar.idleInhibitor.thunderboltDevices = [
     "Caldigit.*"
   ];
   ```

4. Verify: while docked `set-idle-inhibitor status` reports `on` without
   manual action; the waybar eye icon is open; after undocking the
   inhibitor turns off again (unless manually activated).

Introduced with commit ff30c17d93 (waybar: trigger idle inhibitor from
Thunderbolt connect events); the placeholder config was dropped again
in the warning-fix pass because an empty list is a no-op.

## Triage 2026-10-31

Human+hardware-bound; not actionable from an agent sandbox.

Checked:

- Config state: the option exists
  (`modules/myconfig.desktop.wayland/programs.waybar/idleInhibitor.nix`,
  default `[]`); `hosts/host.f13/default.nix` does not set it. The
  placeholder from ff30c17d93 was removed in 8dda0cefda and never
  re-filled — the tracked work is still open.
- Remote collection: `ssh f13` not possible — DNS resolves f13 to
  192.168.1.180 (hosts/metadata.json also lists 192.168.1.155 and
  wireguard 10.199.199.12), but port 22 is refused on all three
  (host powered off or away). No other remote path to
  `/sys/bus/thunderbolt`.
- Beads: no bead tracks this note.
- `nix eval` of test-f13 does not work from the mysbx agent sandbox
  (read-only /nix/store; worktree owned by uid 1000 while the sandbox
  runs as uid 0; full flake eval OOM-killed in the 8GB guest) — the
  eval part of step 3/4 must run on real hardware or the main checkout.

Pattern output to collect while docked (matches what
sync-tb-idle-inhibitor matches: authorized=1, non-domain devices,
attributes `device_name`, `vendor_name`, `unique_id`):

    for d in /sys/bus/thunderbolt/devices/*; do
      [ -f "$d/authorized" ] || continue
      [ "$(cat "$d/authorized")" = 1 ] || continue
      echo "$d: name=$(cat "$d/device_name") vendor=$(cat "$d/vendor_name") unique_id=$(cat "$d/unique_id")"
    done

Next check: when f13 is powered on and docked, run the loop above on
the host (locally or via ssh), fill in the option in
`hosts/host.f13/default.nix`, then eval `test-f13` from the main
checkout and verify per step 4.

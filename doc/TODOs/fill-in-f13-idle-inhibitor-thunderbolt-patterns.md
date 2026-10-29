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

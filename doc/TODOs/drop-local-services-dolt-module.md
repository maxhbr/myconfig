# Drop the local services.dolt module once nixpkgs ships one

`modules/services.dolt.nix` (added alongside `hosts/host.thing/services.dolt.nix`)
defines a local `services.dolt` NixOS module because nixpkgs only packages
the binary (`pkgs/by-name/do/dolt`), with no service module in
`nixos/modules/`. The module runs a `dolt sql-server` as the bd (beads)
server-mode backend on host.thing, bound to the wg0 address.

## What to do

Check whether nixpkgs has gained a Dolt service module after a nixpkgs
bump:

    grep -rli dolt "$(nix eval --raw nixpkgs#path 2>/dev/null)"/nixos/modules/services/ 2>/dev/null

or against the pinned input:

    grep -rli dolt "$(./get_input.sh nixpkgs)"/nixos/modules/services/ 2>/dev/null

An upstream module would live at `nixos/modules/services/databases/dolt.nix`
and expose `services.dolt.*` options. If one exists:

1. Delete `modules/services.dolt.nix`.
2. In `hosts/host.thing/services.dolt.nix`, switch to the upstream
   options, preserving: bind address = wg0 IP
   (`myconfig.metadatalib.getWgIp config.networking.hostName`), port 3307,
   data dir `/var/lib/dolt`, the static `dolt` user with
   `/var/lib/dolt` owned by it (a `DynamicUser`-style rotating uid breaks
   write access to data files restored from a bd backup), and the
   `networking.firewall.interfaces."wg0".allowedTCPPorts` rule.
3. Verify with
   `nix eval --raw .#nixosConfigurations.test-thing.config.system.build.toplevel.drvPath`
   and a `bd dolt test` from a wg0 peer with
   `BEADS_DOLT_SERVER_HOST=thing.wg0.maxhbr.local BEADS_DOLT_SERVER_PORT=3307`.

If the upstream module cannot express the static user or the wg0-only
binding, keep the local module and extend this note with the reason.

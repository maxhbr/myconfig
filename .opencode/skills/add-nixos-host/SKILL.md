---
name: add-nixos-host
description: Create a new NixOS host configuration in this repository
license: MIT
metadata:
  audience: nixos-admins
  workflow: host-creation
---

## What I do

Create a complete new host configuration for this NixOS flake repository including:

1. Host directory structure (`hosts/host.<hostname>/`)
2. `default.nix` with boilerplate configuration
3. `hardware-configuration.nix` template
4. Update `flake.nix`: add `nixosConfigurationsGen.host-<name>` generator and `nixosConfigurations.<name>` entry
5. Update `hosts/metadata.json`: add host metadata entry
6. Set up network and WireGuard configuration (optional)

## When to use me

Use this when adding a new NixOS host to your configuration repository. I'll guide you through gathering all necessary information and create the appropriate files with proper formatting.

## Questions I'll ask

1. **Hostname** - lowercase alphanumeric name (e.g., `newhost`)
2. **System architecture** - `x86_64-linux` or `aarch64-linux`
3. **Host template** - Choose existing host to base configuration on:
   - `f13` (laptop, desktop, wayland, full-featured)
   - `pi4` (headless ARM, minimal, containerized)
   - `spare` (laptop, build slave, minimal desktop)
   - `workstation` (desktop, gaming, AMD/NVIDIA)
   - `nas` (server, headless, storage)
4. **Network configuration**
   - Network name: `home`
   - Use DHCP or static IP?
   - If static: IP address (e.g., `192.168.1.X`)
5. **WireGuard** - Enable WireGuard VPN?
   - If yes: WG IP (e.g., `10.199.199.X`)
6. **SSH keys** - Generate placeholder or reference existing host?

## Implementation steps

1. Validate hostname format (lowercase alphanumeric, no special chars)
2. Read `hosts/metadata.json` to check for duplicates
3. Create host directory: `hosts/host.<hostname>/`
4. Generate `default.nix` based on selected template
5. Generate `hardware-configuration.nix` template (placeholder for actual hardware scan)
6. Add to `flake.nix`:
   - Generator entry in `nixosConfigurationsGen`
   - Configuration entry in `nixosConfigurations`
7. Update `hosts/metadata.json` with host metadata
8. Run `./nixfmtall.sh` to format all changes
9. Validate with `nix flake check`
10. Add new files to git: `git add hosts/host.<hostname>/ flake.nix hosts/metadata.json`

## Notes

- The `hardware-configuration.nix` will be a template; run `sudo nixos-generate-config` on the actual host to generate the real hardware configuration
- Host configurations follow the pattern: `evalConfiguration "<system>" "<hostname>" [ self.nixosModules.core ] ++ moreModules { }`
- New hosts with `announceOtherHosts` will be discoverable by other hosts in the network
- WireGuard IPs should be from the `10.199.199.0/24` subnet
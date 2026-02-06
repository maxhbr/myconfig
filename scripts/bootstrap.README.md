# Bootstrap Instructions

This guide explains how to bootstrap a new NixOS system using the myconfig configuration.

## 1. Clone the Repository

First, clone the myconfig repository onto your live system:

```bash
git clone https://github.com/maxhbr/myconfig.git
cd myconfig
```

## 2. Prepare Disk Partitions

Run the bootstrap script to set up disk partitions and filesystems:

```bash
sudo BOOTSTRAP=YES scripts/bootstrap.sh /dev/sdX "your-passphrase" [vg_name] [mnt]
```

**Example for ephemeral root (stateless system):**

```bash
sudo BOOTSTRAP=YES EPHEMERAL_ROOT=true scripts/bootstrap.sh /dev/nvme0n1 "my-password"
```

This creates a temporary root filesystem and separate persistent subvolumes typically useful for servers or stateless systems.

Parameters:
- `/dev/sdX`: The target device (e.g., `/dev/sda`, `/dev/nvme0n1`)
- `"your-passphrase"`: Disk encryption passphrase (leave empty for no encryption)
- `vg_name`: LVM volume group name (default: `vg`)
- `mnt`: Mount point (default: `/mnt`)

This script will:
- Create partition tables (GPT for EFI, MBR for legacy boot)
- Set up optional LUKS encryption
- Create BTRFS or LVM filesystems with proper subvolumes
- Generate initial NixOS configuration in `/mnt/etc/nixos/`

Optional environment variables:
- `BTRFS=true`: Use BTRFS filesystem (default: `true`)
- `EFI=true`: Force EFI boot mode (default: auto-detect)
- `EPHEMERAL_ROOT=true`: Create ephemeral root setup for stateless systems

## 3. Initialize Private Configuration (Optional)

If you want to maintain custom configuration separate from the main repository:

```bash
scripts/init-priv.sh
```

This creates a `../priv` directory containing a git repository with the private configuration template. This allows you to:
- Keep host-specific secrets and customizations
- Update the template via git subtree (merge-conflict based updates)
- Maintain a parallel git repository next to myconfig

To test without creating the actual directory:
```bash
scripts/init-priv.sh --test
```

### Updating Private Configuration

After the myconfig template has been updated, you can pull new changes into your private config:

```bash
scripts/init-priv.sh update
```

This will:
1. Create a backup at `../priv.<timestamp>` (never automatically cleaned up)
2. Create a fresh subtree branch from the updated template
3. Merge it into your `../priv` repository via git pull

You may encounter merge conflicts that need to be resolved manually. If something goes wrong, you can restore from the backup:

```bash
rm -rf ../priv
cp -a ../priv.<timestamp> ../priv
```

## 4. Install NixOS

Finally, install the NixOS configuration:

### Local Installation
```bash
sudo BOOTSTRAP=YES scripts/bootstrap.nixos-install.sh <hostname>
```

This builds the system configuration for `<hostname>` and installs it to `/mnt`.
`<hostname>` must match a host defined in the flake (e.g., `f13`, `workstation`, `nas`, etc.).

### Remote Installation
To build on one machine and install to another:

```bash
scripts/bootstrap.nixos-install.sh --send <target-host-ip> <hostname>
```

This builds the configuration and sends it to the target host via `nix-copy-closure`. On the target host, run:

```bash
sudo nixos-install --no-root-passwd --system <result-path>
```

The script will print the exact command to run.

## Important Notes

- The `bootstrap.nixos-install.sh` script requires `/mnt/etc/nixos/` to exist, which is created by `bootstrap.sh`
- Set `BOOTSTRAP=YES` environment variable before running bootstrap scripts
- Host-specific configurations are defined in the flake under `nixosConfigurations.*`
- For new hosts, you may need to add host configuration to the flake first

## Troubleshooting

Check disk layout after partitioning:
```bash
sudo fdisk -l /dev/sdX
lsblk
```

Verify mounted filesystems:
```bash
findmnt
```

Review the generated NixOS configuration:
```bash
cat /mnt/etc/nixos/configuration.nix
```
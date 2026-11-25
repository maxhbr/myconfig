# vserver: switch the encrypted swap device to by-partuuid

## What is in place

`hosts/host.vserver/hardware-configuration.nix` enables
`swapDevices = [ { device = "/dev/sda2"; randomEncryption = true; } ]`.
Swap is opened through a `cryptsetup plainOpen` mapping keyed from
`/dev/urandom` on every boot, so its contents never survive a reboot.

The generated `swapDevices = [ { device = "/dev/disk/by-uuid/<uuid>"; } ]`
was replaced, not amended: nixpkgs asserts against `by-uuid`/`by-label`
with `randomEncryption`
(`nixos/modules/config/swap.nix`, the `randomEncryption.enable ->
match "/dev/disk/by-(uuid|label)/" == null` assertion), because the plain
mapping erases the partition's UUID and label on each boot.

## Verified layout (2026-11-25, on vserver)

`lsblk -f` confirms the assumption the change rests on:

```
NAME   FSTYPE FSVER LABEL UUID                                 FSAVAIL FSUSE% MOUNTPOINTS
sda
├─sda1 ext4   1.0   root  deeaa307-2869-468d-86b1-54e87a84d841    2.6G    88% /
└─sda2 swap   1           37c71082-1377-474e-a5a0-f4988dbacf1b                [SWAP]
sr0
```

So `/dev/sda2` is the swap partition and the committed device path is
correct. `boot.resumeDevice` is empty, so no hibernation image depends on
the swap partition's identity.

## What to do

1. Read the swap partition's `PARTUUID`:
   ```bash
   lsblk -o NAME,FSTYPE,PARTUUID /dev/sda
   ```
2. Replace `/dev/sda2` with `/dev/disk/by-partuuid/<uuid>` in
   `hosts/host.vserver/hardware-configuration.nix`. Only
   `by-uuid`/`by-label` are rejected by the assertion, so `by-partuuid` is
   accepted, and unlike a kernel device name it survives renumbering.
3. `nixos-rebuild switch`, then confirm:
   ```bash
   swapon --show                 # NAME is /dev/mapper/dev-...
   cryptsetup status dev-sda2    # type: PLAIN
   lsblk -o NAME,TYPE,FSTYPE     # swap sits on a crypt device
   ```
4. Delete this file.

Between steps 2 and 3 the swap UUID/label still exist on disk; after the
first switch with `randomEncryption` the plain mapping no longer writes
them, which is exactly why a `by-partuuid` (not `by-uuid`) reference is
the durable one.

## If the size is not what you expect

The swap partition is reused at whatever size it already has; the option
neither grows nor shrinks it. If more swap is wanted, setting
`swapDevices = [ { device = "/swapfile"; size = 4096; randomEncryption = true; } ]`
also works (nixpkgs `cryptsetup plainOpen`s the file and `mkswap`s the
mapper), but a swapfile on the unencrypted root inherits root's lack of
encryption — see the encryption-at-rest work for that.

## Related

The rest of the encryption-at-rest question (root, host keys, the
wireguard private key, the step-ca intermediate signing key) is *not*
addressed here and is tracked separately in bd `myconfig-uwxm`. Encrypted
swap only removes the swap-derived leak.

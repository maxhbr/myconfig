# U-Boot-based installation (legacy, not currently functional)

> [!WARNING]
> The legacy U-Boot / SD-image build path (`sdImage`) has been **removed** from
> `hardware-configuration.nix` in favour of the EDK2/UEFI firmware path.
> `flash-sd-card.sh` has been deleted. The instructions below are retained
> for reference only and **will not work** without restoring the `sdImage`
> module configuration.

## What was removed

The previous approach built a complete NixOS SD image with U-Boot baked in
via `sd-image.nix` and `base.nix` imports, then flashed it directly to
microSD or eMMC. This required:

- `hardware-configuration.nix` importing `sd-image.nix` and configuring
  `sdImage.postBuildCommands` (U-Boot fusing at sector offset 64).
- A `flash-sd-card.sh` helper that built `config.system.build.sdImage` and
  flashed it with `dd`.

All of these have been removed in the EDK2 migration.

## If you need the U-Boot path

To restore the legacy U-Boot approach, recover the `sdImage` configuration
from git history:

```bash
git show 50e68889c1:hosts/host.t6/hardware-configuration.nix
git show 50e68889c1:hosts/host.t6/flash-sd-card.sh
```

Then re-add the `sd-image.nix`/`base.nix` imports, `sdImage.*` settings, and
the `flash-sd-card.sh` script.

## Why EDK2 is preferred

See [README.md](./README.md) for the primary EDK2/UEFI installation path.
EDK2 exposes the board as a standard UEFI AArch64 system, enabling the
normal NixOS installer flow (ISO → format target → `nixos-install`) without
requiring a board-specific SD image build.

## References

- FriendlyElec NanoPC-T6 wiki: <https://wiki.friendlyelec.com/wiki/index.php/NanoPC-T6>
- EDK2 firmware for RK3588: <https://github.com/edk2-porting/edk2-rk3588>
- U-Boot Rockchip documentation: <https://docs.u-boot.org/en/latest/board/rockchip/rockchip.html>

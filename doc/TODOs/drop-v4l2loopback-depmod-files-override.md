# Drop the v4l2loopback depmod-files override

Override in `flake.pkgs_overrides.nix`: `fixV4l2loopback`, applied via
`.extend` to `linuxPackages`, `linuxPackages_latest` and `linuxPackages_6_18`;
it appends `rm -f "$out"/lib/modules/*/modules.*` to the `postInstall` of
`v4l2loopback`.

At `nixpkgs` 09063bd1f2b36fd9fd793c36dfade7bd40a93dfc (and `master` at the
time) the `v4l2loopback` output ships a full depmod set
(`lib/modules/<ver>/modules.alias`, `modules.dep`, ...), and the module tree
`linux-<ver>-modules` (`pkgs/os-specific/linux/kmod/aggregator.nix`, a
buildEnv) fails:

```
pkgs.buildEnv error: two given paths contain a conflicting subpath:
  `…-v4l2loopback-0.15.4/lib/modules/7.2.11/modules.alias' and
  `…-linux-7.2.11-modules/lib/modules/7.2.11/modules.alias'
```

## What to do

1. Check whether upstream still ships the files:

   ```bash
   p=$(nix eval --raw .#nixosConfigurations.test-f13.pkgs.nixos-unstable.linuxPackages_latest.v4l2loopback.outPath)
   nix store ls --store https://cache.nixos.org -R "$p" | grep 'lib/modules/[^/]*/modules\.'
   ```

2. Once that prints nothing, delete the overlay and its row in the header
   table of `flake.pkgs_overrides.nix`.
3. Until then, a host that selects another `linuxPackages_*` set with
   `myconfig.v4l2.enable` (or OBS's virtual camera) needs that set added to
   the `genAttrs` list.
4. Verify:

   ```bash
   nix build .#nixosConfigurations.test-f13.config.system.modulesTree
   ```

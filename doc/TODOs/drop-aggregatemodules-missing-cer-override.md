# Drop the aggregateModules missing-certificate override

Override in `flake.pkgs_overrides.nix`: `aggregateModules` wraps upstream's and
rewrites one line of its `postBuild`, replacing

```
| xargs -d'\n' cat >> modules.cer
```

with a loop that `cat`s only the `<module>.ko.cer` files that exist. The
replacement is asserted (`lib.hasInfix`); if upstream rewrites that line, the
evaluation throws instead of silently skipping the override.

At `nixpkgs` 09063bd1f2b36fd9fd793c36dfade7bd40a93dfc (and `master` at the
time), `pkgs/os-specific/linux/kmod/aggregator.nix` merges the single-module
certificate of every external module into `modules.cer`. Modules installed
without the kernel's `modules_install` signing step have none —
`framework-laptop-kmod` copies an unsigned `framework_laptop.ko` by hand — and
the module tree fails:

```
cat: extra/framework_laptop.ko.cer: No such file or directory
```

The kernel config sets `MODULE_SIG` but not `MODULE_SIG_FORCE`, so an unsigned
module loads without a certificate.

## What to do

1. Drop the override once upstream tolerates unsigned external modules, i.e.
   either the aggregator skips missing certificates or `framework-laptop-kmod`
   ships a `framework_laptop.ko.cer`:

   ```bash
   d=$(./get_input.sh nixos-unstable)
   grep -n 'modules.cer' "$d/pkgs/os-specific/linux/kmod/aggregator.nix"
   grep -n 'cer\|modules_install' "$d/pkgs/os-specific/linux/framework-laptop-kmod/default.nix"
   ```

2. Delete the overlay and its row in the header table of
   `flake.pkgs_overrides.nix`.
3. Verify (f13 carries `framework-laptop-kmod`):

   ```bash
   nix build .#nixosConfigurations.test-f13.config.system.modulesTree
   ```

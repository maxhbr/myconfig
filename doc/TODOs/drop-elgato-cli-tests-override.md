# Drop the elgato CLI-tests override

Override in `flake.pkgs_overrides.nix`: a `pythonPackagesExtensions` entry
adding `tests/cli/test_cli.py` to `disabledTestPaths` of `elgato` (6.1.1).

At `nixpkgs` 09063bd1f2b36fd9fd793c36dfade7bd40a93dfc (and `master` at the
time) the test collection fails with `No module named 'click'` (typer no
longer pulls click in); adding `click` is not enough, `test_cli_structure`
then asserts `isinstance(<TyperGroup>, click.Group)`, which current typer
fails. Consumer here: `hosts/host.f13/mykeylight` (library only, no CLI).

## What to do

1. Drop the override once upstream builds again:

   ```bash
   nix build .#nixosConfigurations.test-f13.pkgs.nixos-unstable.python3Packages.elgato
   ```

2. Delete the overlay and its row in the header table of
   `flake.pkgs_overrides.nix`.
3. Verify:

   ```bash
   nix build .#nixosConfigurations.test-f13.pkgs.python3Packages.elgato
   ```

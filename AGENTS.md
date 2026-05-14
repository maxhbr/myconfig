# AGENTS.md

This file contains guidelines for agentic coding agents working on this NixOS flake configuration repository.

## Build/Lint/Test Commands

### Core Nix Commands
- `nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel` - Build a specific host configuration
- `nix build .#x86_64-linux.myconfig-iso` - Build ISO image
- `nix flake check` - Validate all flake outputs across all systems
- `nixDevelop --impure` - Enter development environment (requires --impure flag)

### Building Single home-manager Packages
- `./build-pkg-for-host.sh <pkg-name> [<hostname>]` - Build a single package as
  it is configured in `home.packages` for user `mhuber` on the given host.
  Hostname defaults to the current machine's hostname. Useful for iterating on
  bubblewrap/jail wrappers and other module-generated wrappers without
  rebuilding the whole system or home-manager closure.
  Example: `./build-pkg-for-host.sh jailed-pi f13`

### Formatting & Linting
- `./nixfmtall.sh` - Format all Nix files (runs `nix fmt`)
- `./nixfmtall.sh --check` - Check Nix formatting without making changes
- For shell scripts: `shfmt -d -s -i 4 -ci <file>` - Check shell script formatting (4 space indent)
- `shellcheck -x <file>` - Lint shell scripts

### Pre-commit Hooks
Pre-commit checks run automatically via git-hooks:
- nixfmt-rfc-style (Nix formatting)
- shfmt (shell script formatting, simplified, case indent)
- shellcheck (shell static analysis)
- typos (spell checking)

Run manually: `nix run .#checks.pre-commit-check`

### CI Validation
The CI workflow runs these checks:
- `nix flake check`
- `./nixfmtall.sh --check`
- Dry-run builds for hosts: f13, workstation, nas, spare, vserver

### Finding Build Log Files
Build log files for each host are stored in the parent directory:
- Use symlink: `../result.<hostname>.log` (e.g., `../result.f13.log`)
- Direct access: `../_logs/YYYY-MM-DD-myconfig-<hostname>.log`
- The logs directory `../_logs/` contains historical build logs for all hosts

## Refactoring & Snapshot Verification

When refactoring a module that should be behavior-preserving (no observable
changes to evaluated config), capture a *snapshot* of the relevant slice of
the evaluated NixOS configuration **before** making changes, then diff against
the same query **after**. A byte-identical diff is strong evidence that the
refactor did not alter behavior.

### Workflow
1. Identify which hosts actually exercise the module being refactored
   (grep for the option / import path, e.g. `myconfig.ai.llama-cpp`).
2. Pick the smallest slice of `config` that captures the module's outputs.
   Common targets:
   - The service config it produces, e.g. `config.services.<name>.settings`
   - Generated `home.packages` names and outPaths
   - `myconfig.ai.localModels` or similar registries it contributes to
   - The full toplevel drv hash (coarse but exhaustive — see below)
3. Save the baseline JSON to `/tmp/opencode/<task>/before-<host>.json`.
4. Perform the refactor (split files, rename helpers, etc.).
5. `git add` the new files — `nix` evaluates from the git tree, so untracked
   files are invisible. Forgetting this produces misleading "file does not
   exist" errors.
6. Re-run the same `nix eval` into `after-<host>.json`.
7. `diff before-<host>.json after-<host>.json` → must be empty.

### Snapshot template
For module-specific config + generated home-manager wrappers:
```bash
mkdir -p /tmp/opencode/<task>
nix eval --impure --raw --expr '
let
  flake = builtins.getFlake ("git+file://" + toString /home/mhuber/myconfig/myconfig);
  cfg = flake.nixosConfigurations."<hostname>";

  # --- pick the slices that matter for the module under refactor ---
  serviceSettings = cfg.config.services.<name>.settings;
  hmPkgs = cfg.config.home-manager.users.mhuber.home.packages;
  relevantPkgs = builtins.filter
    (p: let n = p.name or p.pname or ""; in
        builtins.match "<regex-of-generated-pkg-names>.*" n != null)
    hmPkgs;

  # Strip non-JSON-serialisable fields (functions, derivations) from
  # nested attrsets before toJSON. Keep stable identifying fields.
  sanitize = x: { inherit (x) name port; models = x.models or []; };
in
  builtins.toJSON {
    settings = serviceSettings;
    pkgNames = map (p: p.name or p.pname) relevantPkgs;
    pkgOutPaths = map (p: p.outPath) relevantPkgs;
    # add more slices as needed
  }
' > /tmp/opencode/<task>/before-<hostname>.json 2> /tmp/opencode/<task>/before-<hostname>.err
```
After refactoring + `git add`, re-run with `after-<hostname>.json` and:
```bash
diff /tmp/opencode/<task>/before-<hostname>.json \
     /tmp/opencode/<task>/after-<hostname>.json \
  && echo IDENTICAL
```

### Coarser alternative: toplevel drvPath
For a single-line "did anything change at all?" check, compare the system
toplevel derivation path. If it matches, *nothing* about the host changed:
```bash
nix eval --raw .#nixosConfigurations.<hostname>.config.system.build.toplevel.drvPath
```
This is the strongest possible check but gives no signal about *what*
diverged when it does change — use the JSON snapshot to localise diffs.

### Common pitfalls
- **Untracked files**: `nix` reads the git tree (dirty or clean), so
  `git add` every new file before re-evaluating, otherwise the new modules
  are silently invisible and the "after" eval still uses the old layout
  or errors with "path does not exist".
- **Non-serialisable values**: `builtins.toJSON` will fail on functions or
  derivations nested inside attrsets. Strip them via a `sanitize` helper
  (keep only the stable identifying fields like `name` / `port` / `outPath`).
- **Eval warnings vs errors**: a non-zero exit + zero-byte output JSON means
  the eval *failed* — read `*.err`. Pure warnings (e.g. deprecated options)
  appear on stderr but exit 0 and produce valid JSON; that's fine.
- **Latent bugs**: if the original code has a bug (e.g. writes `args` but
  consumers read `params`), the snapshot will encode that buggy behavior.
  Preserve it verbatim in the refactor for a clean diff, and leave a `NOTE`
  comment pointing at the bug for a follow-up commit.
- **Closure-equal output paths**: when extracting a helper that builds a
  derivation, make sure the inputs are identical — even reordering
  `runtimeInputs` or changing whitespace inside a `writeShellApplication`
  text changes the outPath and breaks the diff.

## Git Hygiene

### Adding New Files
- **Always add newly created files to git** after they are created and validated
- Run `git add <filepath>` for each new file (e.g., `git add hosts/host.newmachine/default.nix`)
- For new hosts, add the entire directory: `git add hosts/host.<hostname>/`
- Update `flake.nix` and metadata files should also be staged
- This ensures all changes are tracked and visible via `git status`

### Before Committing
- Run `./nixfmtall.sh` to format all Nix files
- Run `nix flake check` to validate configuration
- Review staged changes with `git diff --staged`
- Only commit when explicitly requested by the user

## Code Style Guidelines

### Nix Files
- **Formatting**: Use nixfmt-rfc-style (RFC 51 style)
- **File naming**: Use dot-separated names following module hierarchy
  - Examples: `services.openssh.nix`, `myconfig.desktop.nix`, `dev.haskell/default.nix`
- **Module structure**: Follow standard NixOS module pattern
  ```nix
  { config, lib, pkgs, ... }:
  let
    cfg = config.myconfig.<feature>;
  in
  {
    options.myconfig.<feature> = with lib; {
      enable = mkEnableOption "myconfig.<feature>";
    };
    config = lib.mkIf cfg.enable {
      # Configuration here
    };
  }
  ```

- **Imports**: For modular features, use `imports = [ ./submodule.nix ];` pattern
- **Conditionals**: Use `lib.mkIf` for conditional configuration
- **Options**: Always define options before config section with `mkEnableOption` for booleans

### Shell Scripts
- **Formatting**: 4-space indentation, simplified formatting, case indentation
- **Shebang**: `#!/usr/bin/env bash`
- **Error handling**: `set -euo pipefail` at script start
- **Shellcheck directives**: Add `# shellcheck disable=SC<code>` for necessary exceptions
- **Dependencies**: Use nix-shell shebang for reproducible: `#! nix-shell -i bash -p <packages>`

### General Patterns
- **_flake.nix_**: Contains outputs defined per system using `eachDefaultSystem`
- **Nixpkgs overlays**: Use overlays for custom packages or version pinning
- **Modules organization**: Group by feature (e.g., `myconfig.desktop.*`, `dev.*`, `services.*`)
- **Host configurations**: Use `nixosConfigurationsGen.host-<name>` pattern for code reuse
- **Metadata**: Host metadata stored in `hosts/metadata.json`

### Imports & Dependencies
- Always import Nixpkgs from inputs: `inputs.nixpkgs.legacyPackages.${system}`
- Use `inherit (inputs.nixpkgs) lib` for nixpkgs lib functions
- Follow flake input convention: define all inputs at top of _flake.nix_

### Local Source Checkouts for Lookup
Local checkouts of upstream sources are available for reference and lookup
(e.g. searching for module options, package definitions, or library functions):
- **nixpkgs**: `~/myconfig/nixos/nixpkgs`
- **home-manager**: `~/myconfig/nixos/home-manager`

Use these checkouts with Grep/Glob/Read tools to investigate upstream behavior,
find existing modules, or look up option definitions. Do not modify these
directories — they are read-only references.

### Naming Conventions
- Modules: `category.subcategory.feature.nix` or `subdir/default.nix`
- Options: `myconfig.<category>.<feature>.enable`
- Context variables: `cfg` for current config, `self/super` for overlays
- Host names: lowercase alphanumeric (e.g., f13, workstation, nas)

### Security & Privacy
- Use agenix for secrets management
- Never commit secrets to repository
- Git used for sensitive files (via git-crypt, git-secrets)
- **All secrets must be stored in the separate `../priv/` repository, never in this repo**

### Error Handling
- Nix: Use `lib.mkIf` for conditional logic rather than throwing errors
- Shell: Use `set -euo pipefail` for robust error handling
- Build failures always logged and surfaced

### Attribution
Consider adding copyright headers to new files:
```nix
# Copyright <year> Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
```

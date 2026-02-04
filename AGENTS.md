# AGENTS.md

This file contains guidelines for agentic coding agents working on this NixOS flake configuration repository.

## Build/Lint/Test Commands

### Core Nix Commands
- `nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel` - Build a specific host configuration
- `nix build .#x86_64-linux.myconfig-iso` - Build ISO image
- `nix flake check` - Validate all flake outputs across all systems
- `nixDevelop --impure` - Enter development environment (requires --impure flag)

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

### Naming Conventions
- Modules: `category.subcategory.feature.nix` or `subdir/default.nix`
- Options: `myconfig.<category>.<feature>.enable`
- Context variables: `cfg` for current config, `self/super` for overlays
- Host names: lowercase alphanumeric (e.g., f13, workstation, nas)

### Security & Privacy
- Use agenix for secrets management
- Never commit secrets to repository
- Git used for sensitive files (via git-crypt, git-secrets)

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

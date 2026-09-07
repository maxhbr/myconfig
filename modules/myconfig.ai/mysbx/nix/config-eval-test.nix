# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# A module-EVALUATION test of the user configuration layer
# ../default.nix generates (review-4 item 4).
#
# The Rust suite in ../mysbx-rs/tests/ covers what the CLI does with a
# `config.toml`, but it hand-writes that file: a regression in the
# GENERATOR — a dropped `[env]` entry, a mount whose `dest` stops
# pointing below `/mysbx-home`, a baseline value a host can no longer
# override — is invisible to it. This check evaluates the module itself,
# in a handful of scenarios, and asserts on the bytes of the generated
# `mysbx/config.toml`.
#
# Scenarios (see `scenarios` below): Home Manager ripgrep on with
# arguments, on without arguments, off, a host overriding the baseline
# `RIPGREP_CONFIG_PATH`, and a host adding an unrelated `[env]` key.
{
  inputs,
  system,
}:
let
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  lib = inputs.nixpkgs.lib;

  # The smallest NixOS + Home Manager evaluation that carries the mysbx
  # module. Nothing here is ever built: the check reads
  # `xdg.configFile."mysbx/config.toml".source`, which is a
  # `pkgs.formats.toml` derivation of the generated attrset.
  generated =
    extraModules:
    (lib.nixosSystem {
      inherit system;
      modules = [
        inputs.home.nixosModules.home-manager
        ../default.nix
        {
          nixpkgs.hostPlatform = system;
          system.stateVersion = "24.05";
          boot.loader.grub.enable = false;
          fileSystems."/" = {
            device = "none";
            fsType = "tmpfs";
          };
          users.users.mhuber = {
            isNormalUser = true;
            home = "/home/mhuber";
          };
          home-manager.users.mhuber.home.stateVersion = "24.05";
          myconfig.ai.mysbx.enable = true;
        }
      ]
      ++ extraModules;
    }).config.home-manager.users.mhuber.xdg.configFile."mysbx/config.toml".source;

  ripgrepOn = {
    home-manager.users.mhuber.programs.ripgrep = {
      enable = true;
      arguments = [ "--smart-case" ];
    };
  };

  scenarios = {
    # Ripgrep active: the file Home Manager writes exists, so both the
    # read-only mount and the variable pointing at its in-sandbox copy
    # must be generated.
    rgOn = generated [ ripgrepOn ];
    # The gate is `enable && arguments != []`: without arguments Home
    # Manager writes no `ripgreprc`, and a variable pointing at a
    # missing file is a hard `rg` failure.
    rgNoArgs = generated [
      { home-manager.users.mhuber.programs.ripgrep.enable = true; }
    ];
    rgOff = generated [ { } ];
    # A host (or a per-agent module) setting the SAME key must win,
    # without an evaluation conflict: the baseline is defined per key
    # with `mkDefault`.
    rgOverridden = generated [
      ripgrepOn
      { myconfig.ai.mysbx.config.env.RIPGREP_CONFIG_PATH = "/mysbx-home/custom/ripgreprc"; }
    ];
    # An unrelated key merges with the baseline instead of replacing it
    # (which a whole-attrset `mkDefault` would do).
    rgPlusExtra = generated [
      ripgrepOn
      { myconfig.ai.mysbx.config.env.MYSBX_EVAL_TEST = "extra-value"; }
    ];
  };
in
pkgs.runCommand "mysbx-generated-config-test"
  {
    inherit (scenarios)
      rgOn
      rgNoArgs
      rgOff
      rgOverridden
      rgPlusExtra
      ;
  }
  ''
    fail() {
      echo "mysbx generated-config test: $1" >&2
      echo "--- generated file ---" >&2
      cat "$2" >&2
      exit 1
    }

    # 1. ripgrep enabled with arguments: the ro mount below
    #    /mysbx-home AND the variable naming the file inside it.
    grep -q 'path = "~/.config/ripgrep"' "$rgOn" \
      || fail "the ripgrep mount source is missing" "$rgOn"
    grep -q 'dest = "/mysbx-home/.config/ripgrep"' "$rgOn" \
      || fail "the ripgrep mount dest is not below /mysbx-home" "$rgOn"
    grep -q 'mode = "ro"' "$rgOn" \
      || fail "the ripgrep mount is not read-only" "$rgOn"
    grep -q 'RIPGREP_CONFIG_PATH = "/mysbx-home/.config/ripgrep/ripgreprc"' "$rgOn" \
      || fail "RIPGREP_CONFIG_PATH is missing from [env]" "$rgOn"

    # 2. the gate is false: no variable. The unconditional baseline
    #    mount may stay — a mounted directory harms nothing, a variable
    #    pointing at a file Home Manager never wrote does.
    if grep -q RIPGREP_CONFIG_PATH "$rgNoArgs"; then
      fail "RIPGREP_CONFIG_PATH must not be set without arguments" "$rgNoArgs"
    fi
    if grep -q RIPGREP_CONFIG_PATH "$rgOff"; then
      fail "RIPGREP_CONFIG_PATH must not be set with ripgrep disabled" "$rgOff"
    fi

    # 3. a host value for the same key wins, and does not conflict
    #    (an eval conflict would have failed this derivation's inputs
    #    before the script ever ran).
    grep -q 'RIPGREP_CONFIG_PATH = "/mysbx-home/custom/ripgreprc"' "$rgOverridden" \
      || fail "the host override did not win" "$rgOverridden"
    if grep -q 'RIPGREP_CONFIG_PATH = "/mysbx-home/.config/ripgrep/ripgreprc"' "$rgOverridden"; then
      fail "the baseline value survived the override" "$rgOverridden"
    fi

    # 4. an unrelated key MERGES with the baseline.
    grep -q 'MYSBX_EVAL_TEST = "extra-value"' "$rgPlusExtra" \
      || fail "the extra [env] key is missing" "$rgPlusExtra"
    grep -q 'RIPGREP_CONFIG_PATH = "/mysbx-home/.config/ripgrep/ripgreprc"' "$rgPlusExtra" \
      || fail "the baseline was replaced instead of merged" "$rgPlusExtra"

    mkdir "$out"
  ''

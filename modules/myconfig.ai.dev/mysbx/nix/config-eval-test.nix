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
# `RIPGREP_CONFIG_PATH`, a host adding an unrelated `[env]` key, and the
# workmux integration on / off, and the `multiplexer` selection with
# its availability gate (../docs/design/config.md D16/D17).
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
  # The evaluated configuration of the smallest NixOS + Home Manager
  # system that carries the mysbx module. `generated` reads the TOML
  # file out of it; `assertionsOf` reads the module's own assertions,
  # which the generated file cannot show (the availability gate of
  # ../docs/design/config.md D17 is an eval-time refusal, not a config
  # value).
  evaluated =
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
    }).config;

  generated =
    extraModules:
    (evaluated extraModules).home-manager.users.mhuber.xdg.configFile."mysbx/config.toml".source;

  # The messages of the assertions that FAIL in this configuration.
  failedAssertions =
    extraModules:
    map (a: a.message) (builtins.filter (a: !a.assertion) (evaluated extraModules).assertions);

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
    # The workmux integration (D16): the switch and the in-sandbox
    # workmux config mount must appear in the generated layer. The
    # `package` is a stand-in here — this scenario only reads the
    # generated `config.toml`, never the sandbox PATH or the entry
    # script, and pulling the real workmux flake input into a check
    # that asserts on TOML bytes would buy nothing.
    workmuxOn = generated [
      {
        myconfig.ai.mysbx.workmux = {
          enable = true;
          package = pkgs.hello;
          settings.agents.pi = {
            type = "pi";
            command = "pi";
          };
        };
      }
    ];
    # And without it, the selection must be the plain shell: a host
    # without any integration keeps the pre-D16 payload.
    workmuxOff = generated [ { } ];
    # The other selectable multiplexers (D17). `herdr` is available by
    # default (`herdr.package` defaults to `pkgs.herdr`), `aoe` is not
    # (it is gated on ../../programs/programs.agent-of-empires/ being enabled), so
    # the two exercise both halves of the availability gate.
    muxHerdr = generated [
      { myconfig.ai.mysbx.config.multiplexer = "herdr"; }
    ];
    muxTmux = generated [
      { myconfig.ai.mysbx.config.multiplexer = "tmux"; }
    ];
    # A host-wide selection this host cannot start must fail at EVAL
    # time, naming the option to set — not on the first `mysbx` of
    # every sandbox.
    muxUnavailable = failedAssertions [
      { myconfig.ai.mysbx.config.multiplexer = "aoe"; }
    ];
    # ... and a selection that IS available must not produce that
    # assertion (the gate must not fire on the happy path).
    muxAvailableAsserts = failedAssertions [
      { myconfig.ai.mysbx.config.multiplexer = "herdr"; }
    ];
  };

  # The eval-time results are checked HERE, while the check derivation
  # is built: `runCommand` can only see strings and paths, and an
  # assertion list is neither. A failure aborts the build of the check
  # with the message below — the same visibility a shell `fail` has.
  assertionGate =
    let
      unavailable = scenarios.muxUnavailable;
      available = scenarios.muxAvailableAsserts;
      names = builtins.concatStringsSep "\n" unavailable;
    in
    if !(builtins.any (m: lib.hasInfix "multiplexer" m) unavailable) then
      throw "mysbx generated-config test: selecting the unavailable `aoe` multiplexer did not fail an assertion (failed: ${names})"
    else if builtins.any (m: lib.hasInfix "multiplexer" m) available then
      throw "mysbx generated-config test: the availability gate fired for an AVAILABLE multiplexer (failed: ${builtins.concatStringsSep "\n" available})"
    else
      "ok";
in
pkgs.runCommand "mysbx-generated-config-test"
  {
    inherit (scenarios)
      rgOn
      rgNoArgs
      rgOff
      rgOverridden
      rgPlusExtra
      workmuxOn
      workmuxOff
      muxHerdr
      muxTmux
      ;
    inherit assertionGate;
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

    # 5. the workmux integration (D16/D17): the selection, plus the
    #    read-only mount of the in-sandbox workmux config below
    #    /mysbx-home — and the plain shell when it is off.
    grep -q '^multiplexer = "workmux"$' "$workmuxOn" \
      || fail "the workmux selection is missing" "$workmuxOn"
    grep -q 'dest = "/mysbx-home/.config/workmux/config.yaml"' "$workmuxOn" \
      || fail "the in-sandbox workmux config is not mounted" "$workmuxOn"
    grep -q '^multiplexer = "none"$' "$workmuxOff" \
      || fail "the default must be the plain shell without the integration" "$workmuxOff"
    if grep -q workmux "$workmuxOff"; then
      fail "workmux must not appear without the integration" "$workmuxOff"
    fi

    # 6. the other selections (D17) reach the generated layer verbatim,
    #    and the eval-time availability gate was checked while this
    #    derivation was instantiated ($assertionGate).
    grep -q '^multiplexer = "herdr"$' "$muxHerdr" \
      || fail "the herdr selection is missing" "$muxHerdr"
    grep -q '^multiplexer = "tmux"$' "$muxTmux" \
      || fail "the tmux selection is missing" "$muxTmux"
    [ "$assertionGate" = ok ] \
      || { echo "mysbx generated-config test: assertion gate: $assertionGate" >&2; exit 1; }

    mkdir "$out"
  ''

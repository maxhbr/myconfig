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
# `RIPGREP_CONFIG_PATH`, a host adding an unrelated `[env]` key, the
# workmux integration on / off, the `multiplexer` selection with its
# availability gate (../docs/design/config.md D16/D17), and the shared
# `myconfig.ai.dev.sandboxTools` hook (phase 2d): its env must reach
# the generated `[env]` table (its packages flow through
# `extraTools`, which the module merges — they are not visible in this
# TOML).
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
  #
  # `myconfig.ai.dev.sandboxTools`
  # (../../sandboxes/myconfig.ai.sandboxTools.nix) is imported next to
  # mysbx because mysbx consumes the hook (plan.md phase 2d): on real
  # hosts the `myconfig.ai.dev` umbrella imports both, so the minimal
  # evaluation here must do it by hand.
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
        ../../sandboxes/myconfig.ai.sandboxTools.nix
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
          myconfig.ai.dev.mysbx.enable = true;
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

  difftasticExternal = {
    home-manager.users.mhuber.programs.difftastic = {
      enable = true;
      git = {
        enable = true;
        mode = "external";
      };
    };
  };

  difftasticDifftoolOnly = {
    home-manager.users.mhuber.programs.difftastic = {
      enable = true;
      git = {
        enable = true;
        mode = "difftool";
      };
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
      { myconfig.ai.dev.mysbx.config.env.RIPGREP_CONFIG_PATH = "/mysbx-home/custom/ripgreprc"; }
    ];
    # An unrelated key merges with the baseline instead of replacing it
    # (which a whole-attrset `mkDefault` would do).
    rgPlusExtra = generated [
      ripgrepOn
      { myconfig.ai.dev.mysbx.config.env.MYSBX_EVAL_TEST = "extra-value"; }
    ];
    # `GIT_EXTERNAL_DIFF` (bd myconfig-kvo): set exactly when Home
    # Manager activates difftastic's `diff.external` — the override
    # that restores the default unified diff inside the sandbox.
    dftExternal = generated [ difftasticExternal ];
    # `git.mode = "difftool"` leaves `git diff` untouched (only
    # `git difftool` goes through difftastic), so no override is
    # needed and none may be generated.
    dftDifftool = generated [ difftasticDifftoolOnly ];
    # Without difftastic the mounted `~/.config/git` carries no
    # `diff.external`, and the baseline must not rewrite the host's
    # (already default) diff behavior either.
    dftOff = generated [ { } ];
    # A host (or per-agent module) setting the SAME key wins, per the
    # per-key `mkDefault` of the baseline.
    dftOverridden = generated [
      difftasticExternal
      { myconfig.ai.dev.mysbx.config.env.GIT_EXTERNAL_DIFF = "/nix/store/0000custom-diff"; }
    ];
    # The workmux integration (D16): the switch and the in-sandbox
    # workmux config mount must appear in the generated layer. The
    # `package` is a stand-in here — this scenario only reads the
    # generated `config.toml`, never the sandbox PATH or the entry
    # script, and pulling the real workmux flake input into a check
    # that asserts on TOML bytes would buy nothing.
    workmuxOn = generated [
      {
        myconfig.ai.dev.mysbx.workmux = {
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
    # The shared sandbox-tools hook (phase 2d): an `extraEnv` entry of
    # the hook must appear in the generated `[env]` table like any
    # baseline value, and a tier baseline key (RIPGREP_CONFIG_PATH) set
    # by the hook too must resolve to the TIER's value (the `//` in
    # ../default.nix) while the hook's other keys still arrive.
    sandboxToolsEnv = generated [
      ripgrepOn
      {
        myconfig.ai.dev.sandboxTools.extraEnv = {
          MYSBX_HOOK_TEST = "hook-value";
          RIPGREP_CONFIG_PATH = "/hook/clash-must-lose";
        };
      }
    ];
    # ... and without the hook, its key must not appear at all.
    sandboxToolsEnvOff = generated [ ripgrepOn ];
    # The other selectable multiplexers (D17). `herdr` is available by
    # default (`herdr.package` defaults to `pkgs.herdr`), `aoe` is not
    # (it is gated on ../../programs/programs.agent-of-empires/ being enabled), so
    # the two exercise both halves of the availability gate.
    muxHerdr = generated [
      { myconfig.ai.dev.mysbx.config.multiplexer = "herdr"; }
    ];
    muxTmux = generated [
      { myconfig.ai.dev.mysbx.config.multiplexer = "tmux"; }
    ];
    # A host-wide selection this host cannot start must fail at EVAL
    # time, naming the option to set — not on the first `mysbx` of
    # every sandbox.
    muxUnavailable = failedAssertions [
      { myconfig.ai.dev.mysbx.config.multiplexer = "aoe"; }
    ];
    # ... and a selection that IS available must not produce that
    # assertion (the gate must not fire on the happy path).
    muxAvailableAsserts = failedAssertions [
      { myconfig.ai.dev.mysbx.config.multiplexer = "herdr"; }
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
      dftExternal
      dftDifftool
      dftOff
      dftOverridden
      workmuxOn
      workmuxOff
      muxHerdr
      muxTmux
      sandboxToolsEnv
      sandboxToolsEnvOff
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

    # 4a. GIT_EXTERNAL_DIFF (bd myconfig-kvo): present exactly when Home
    #     Manager activates difftastic's diff.external, pointing at the
    #     mysbx-git-default-diff wrapper of the package closure.
    grep -q '^GIT_EXTERNAL_DIFF = "/nix/store/.*-mysbx-git-default-diff/bin/mysbx-git-default-diff"$' "$dftExternal" \
      || fail "GIT_EXTERNAL_DIFF is missing or not the wrapper" "$dftExternal"
    if grep -q GIT_EXTERNAL_DIFF "$dftDifftool"; then
      fail "GIT_EXTERNAL_DIFF must not be set when only the difftool mode is active" "$dftDifftool"
    fi
    if grep -q GIT_EXTERNAL_DIFF "$dftOff"; then
      fail "GIT_EXTERNAL_DIFF must not be set without difftastic" "$dftOff"
    fi
    grep -q '^GIT_EXTERNAL_DIFF = "/nix/store/0000custom-diff"$' "$dftOverridden" \
      || fail "the host override of GIT_EXTERNAL_DIFF did not win" "$dftOverridden"
    if grep -q 'mysbx-git-default-diff' "$dftOverridden"; then
      fail "the baseline GIT_EXTERNAL_DIFF survived the override" "$dftOverridden"
    fi

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

    # 7. the shared sandbox-tools hook (phase 2d): its env entries reach
    #    the generated [env] table, a hook/baseline clash resolves to
    #    the tier's value, and without the hook nothing leaks in.
    grep -q 'MYSBX_HOOK_TEST = "hook-value"' "$sandboxToolsEnv" \
      || fail "the sandboxTools.extraEnv key is missing from [env]" "$sandboxToolsEnv"
    grep -q 'RIPGREP_CONFIG_PATH = "/mysbx-home/.config/ripgrep/ripgreprc"' "$sandboxToolsEnv" \
      || fail "the tier baseline lost the clash against sandboxTools.extraEnv" "$sandboxToolsEnv"
    if grep -q 'MYSBX_HOOK_TEST' "$sandboxToolsEnvOff"; then
      fail "a sandboxTools.extraEnv key appeared without the hook" "$sandboxToolsEnvOff"
    fi

    [ "$assertionGate" = ok ] \
      || { echo "mysbx generated-config test: assertion gate: $assertionGate" >&2; exit 1; }

    mkdir "$out"
  ''

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.hermes — `mysbx` sandbox integration.
#
# Puts the hermes CLI into every `mysbx` sandbox of this user, pointed at the
# same LiteLLM route the host `myconfig.ai.dev.hermes` backends use. Unlike
# pi/opencode (`../programs/programs.pi-coding-agent`,
# `../programs/programs.opencode`) nothing of the host `~/.hermes` is
# mounted: hermes keeps `config.yaml`, `.env`, `auth.json`, `state.db` and
# `sessions/` in ONE root (`hermes_constants.py` `get_hermes_home()`), i.e.
# configuration is inseparable from credentials there. A sandboxed session
# therefore starts from a SEEDED config instead of a host one:
#
#   1. the binary on the sandbox PATH — `myconfig.ai.dev.mysbx.extraTools`
#      for the bwrap/nono/krun backends, and
#      `myconfig.ai.dev.mysbx.podman.imagePackages` for the podman backends
#      (which bake a package's `/bin` into the shared image; nothing from the
#      host store is mounted there).
#   2. `bin/hermes` of that package is a launcher (`hermes-mysbx-seed-config`)
#      that seeds a credentials-free `config.yaml` into `$HERMES_HOME` when
#      there is none, and otherwise REPAIR-MERGES the LiteLLM model route
#      back into an existing config (./seed-cli-config.nix — hermes itself
#      (`hermes setup`/`migrate`) and the aoe hook installer rewrite the
#      persisted config and can drop the route; a seed-only launcher could
#      never heal that). Then it execs the upstream CLI. Seeding from the
#      launcher is the same mechanism `../programs/programs.claude-code/`
#      uses: the sandbox home is a tmpfs, so a store-mounted config file
#      would be either invisible (podman), read-only (`/model`, `config
#      set` would fail like bd myconfig-j6i), or shown as the first-run
#      wizard on every launch.
#   3. `.hermes` as a `state-dirs` entry (`../../mysbx/docs/design/config.md`
#      D15): sessions, memories and the seeded config persist per repository
#      in `<repo>.mysbx/state/.hermes/` instead of dying with the tmpfs home.
#      Persisting the root also persists whatever `hermes auth`/`hermes model`
#      write there — which is why the seed carries only a placeholder key
#      (`local-key`, the value the host backends use for the keyless loopback
#      proxy); the real upstream credential stays in the host LiteLLM proxy.
#      The repair-merge never overrides a non-empty `model.default`, so a
#      `hermes model` choice made in a sandbox survives later launches.
#   4. the podman backends get `CUSTOM_BASE_URL` (`myconfig.ai.dev.mysbx.
#      podman.env`), because a container's loopback is its own: hermes resolves
#      the endpoint as `CUSTOM_BASE_URL` → `config.yaml base_url` →
#      `OPENROUTER_BASE_URL` → openrouter.ai (`hermes_cli/runtime_provider.py`),
#      so the seeded host URL is overridden per backend, exactly like the
#      `MYCONFIG_LITELLM_BASE_URL` override of
#      `../programs/programs.pi-coding-agent`.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  osconfig = config;
  cfg = config.myconfig.ai.dev.hermes;

  hermesPkg = inputs.hermes-agent.packages.${pkgs.stdenv.hostPlatform.system}.default;

  seeding = import ./seed-cli-config.nix {
    inherit lib pkgs;
    baseUrl = cfg.model.baseUrl;
    apiKey = "local-key";
    defaultModel = cfg.model.default;
    # No named `providers` entry in the sandbox: the minimal `model:` route
    # is all `_has_any_provider_configured` needs, and the picker's
    # `providers` view would just duplicate the LiteLLM model list on every
    # launch's merge.
    providerEntry = null;
    externalSkillDirs = cfg.skills.externalDirs;
  };
  inherit (seeding) seed;

  # `bin/hermes` runs the seeder first, then execs the upstream binary with
  # the original argv0. `hermes-agent` / `hermes-acp` stay untouched: the
  # gateway and ACP entry points are not what a sandbox session starts.
  hermesSandboxed = pkgs.symlinkJoin {
    name = "hermes-agent-mysbx-${lib.getVersion hermesPkg}";
    paths = [ hermesPkg ];
    inherit (hermesPkg) meta;
    postBuild = ''
      mv "$out/bin/hermes" "$out/bin/.hermes-upstream"
      cat >"$out/bin/hermes" <<EOF
      #! ${pkgs.runtimeShell} -e
      ${lib.getExe seed} || true
      exec -a "\$0" "$out/bin/.hermes-upstream" "\$@"
      EOF
      chmod +x "$out/bin/hermes"
    '';
    passthru = {
      inherit seed;
      unwrapped = hermesPkg;
    };
  };
in
{
  config = lib.mkIf (cfg.enable && osconfig.myconfig.ai.dev.mysbx.enable) {
    myconfig.ai.dev.mysbx = {
      extraTools = [ hermesSandboxed ];

      # D32 agent registry entry: the launcher above (config seed +
      # exec), NOT `bin/hermes-acp` — an interactive session starts
      # the plain CLI.
      agents.hermes.command = "hermes";

      # The podman backends run the image's own `/bin`, so the launcher has
      # to be baked in rather than added to the sandbox PATH.
      podman = {
        # `mkOptionDefault` merges with the option's default list
        # (the mux entries, waypipe, …) instead of replacing it — the
        # same spelling dev.core uses for its `sandboxCliTools`.
        imagePackages = lib.mkOptionDefault [ hermesSandboxed ];
        env = lib.mkIf osconfig.myconfig.ai.dev.litellm-forwarder.enable {
          CUSTOM_BASE_URL = osconfig.myconfig.ai.dev.litellm-forwarder.endpoint;
        };
      };

      # The litellm endpoint of the microvm-qemu backend (bd
      # myconfig-mzys): the SLiRP gateway `10.0.2.2` maps to the host
      # loopback — the same `CUSTOM_BASE_URL` override as the podman
      # one above, with the qemu-guest URL instead of the forwarder's.
      microvm-qemu.env = lib.mkIf osconfig.services.litellm.enable {
        CUSTOM_BASE_URL = "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
      };
      # The microvm-ch backend's passt guest uses the same address plan.
      microvm-ch.env = lib.mkIf osconfig.services.litellm.enable {
        CUSTOM_BASE_URL = "http://10.0.2.2:${toString osconfig.services.litellm.port}/v1";
      };

      config.stateDirs = [ ".hermes" ];
    };
  };
}

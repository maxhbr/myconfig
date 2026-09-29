# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.mysbx.gvisor — the container image of both podman
# backends (`podman-gvisor`, `podman-krun`) and the host setup they
# need: rootless podman, the subordinate id ranges of the user, the
# pinned gVisor (./nix/gvisor-overlay.nix) and its `runsc` runtime
# registration.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.ai.dev.mysbx;

  # The coding-agent CLIs this repo can install on the host, mapped from
  # their `myconfig.ai.dev.<name>.enable` flag to the package the host
  # wrapper uses (../programs/programs.<name>). Whatever the host enables
  # is baked into the image. `aichat` / `llm` are host-side chat
  # front-ends, not agents, and stay out.
  agentPackagesByFlag = {
    pi-coding-agent = pkgs.nixos-unstable.pi-coding-agent;
    opencode = pkgs.opencode;
    claude-code = pkgs.claude-code;
    codex = pkgs.codex;
    github-copilot-cli = pkgs.github-copilot-cli;
    qwen-code = pkgs.qwen-code;
  };

  enabledAgentPackages = lib.attrValues (
    lib.filterAttrs (name: _: config.myconfig.ai.dev.${name}.enable or false) agentPackagesByFlag
  );

  # The fish runtime the host's rendered fish configuration assumes (bd
  # myconfig-cew), for the read-only `~/.config/fish` mount of
  # `baselineMounts`: fish, the tools its config names by store path
  # (the eza/bat aliases, the `any-nix-shell` hook, grc) and the plugin
  # `src` trees the generated conf.d files load from.
  fishConveniencePackages =
    let
      hm = config.home-manager.users."${myconfig.user}";
    in
    lib.optionals (config.programs.fish.enable && hm.programs.fish.enable) (
      [
        hm.programs.fish.package
        pkgs.any-nix-shell
        pkgs.eza
        pkgs.bat
        pkgs.grc
      ]
      ++ (map (p: p.src) hm.programs.fish.plugins)
    );
in
{
  options.myconfig.ai.dev.mysbx.gvisor = with lib; {
    image = mkOption {
      type = types.nullOr types.package;
      default = pkgs.callPackage ./nix/agent-image.nix {
        extraPackages = cfg.gvisor.extraImagePackages;
        includeNixDB = cfg.krun.nix.enable;
      };
      defaultText = literalExpression ''
        pkgs.callPackage ./nix/agent-image.nix {
          extraPackages = cfg.gvisor.extraImagePackages;
          includeNixDB = cfg.krun.nix.enable;
        }'';
      description = ''
        The Nix-built OCI image both podman backends run, pinned into the
        wrapper together with its reference and expected image ID
        (MYSBX_GVISOR_TARBALL / _IMAGE / _IMAGE_ID, ./nix/mysbx.nix);
        `mysbx podman-load-image` loads it.

        `null` pins nothing and drops the podman host setup of this
        module: `backend = "podman-gvisor"` / `"podman-krun"` are refused
        runs and `mysbx podman-load-image` is a usage error.
      '';
    };

    extraImagePackages = mkOption {
      type = types.listOf types.package;
      default =
        enabledAgentPackages
        ++ lib.optional (enabledAgentPackages != [ ]) pkgs.herdr
        ++ fishConveniencePackages
        ++ cfg.gvisor.imagePackages
        ++ config.myconfig.ai.dev.sandboxTools.extraPackages;
      defaultText = literalExpression ''
        the enabled coding-agent CLIs (plus `pkgs.herdr` when there is
        one), the fish world of the home-manager user when fish is its
        shell, `gvisor.imagePackages` and
        `myconfig.ai.dev.sandboxTools.extraPackages`'';
      description = ''
        Packages baked into the default `image` on top of its base
        toolchain. The base image ships no agent CLI; host binaries are
        never bind-mounted, which would drag the host /nix store into
        the sandbox.
      '';
    };
  };

  config = lib.mkMerge [
    # Not gated on `image`: that default reads `pkgs`.
    (lib.mkIf cfg.enable { nixpkgs.overlays = [ (import ./nix/gvisor-overlay.nix) ]; })
    (lib.mkIf (cfg.enable && cfg.gvisor.image != null) {
      virtualisation.podman.enable = true;

      # `--runtime=runsc` of the podman-gvisor backend; other containers
      # keep podman's default runtime. podman-krun passes crun by path.
      virtualisation.containers.containersConf.settings.engine.runtimes.runsc = [
        "${pkgs.gvisor}/bin/runsc"
      ];

      # Rootless podman needs subordinate id ranges.
      users.users.${myconfig.user}.autoSubUidGidRange = true;

      home-manager.sharedModules = [
        { home.packages = [ pkgs.gvisor ]; }
      ];
    })
  ];
}

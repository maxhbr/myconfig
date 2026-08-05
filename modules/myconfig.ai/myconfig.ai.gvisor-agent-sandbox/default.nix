# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.gvisor-agent-sandbox — rootless Podman + gVisor agent sandboxes.
#
# A container-based isolation tier for coding agents, sitting between the
# bubblewrap `jail-app` wrappers (`myconfig.ai.jail`) and the Cloud Hypervisor
# microVM tier (`myconfig.ai.microvm`). Each session runs in a rootless Podman
# container with gVisor (`runsc`) as OCI runtime, on a Nix-built sandbox image,
# and gets its own git worktree from a disposable bare pool.
#
# The sources under ./bin and ./nix are vendored from
# https://github.com/maxhbr/gvisor-agent-sandbox via `git subtree`; see
# ./README.md for the import commit and how to pull upstream changes. The
# upstream standalone-flake plumbing (flake.nix / flake.lock /
# nixos/agent-sandboxes.nix) was dropped — this module replaces it.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.ai.gvisor-agent-sandbox;

  # The image actually used: either the configured one, or the default with
  # `extraImagePackages` folded in.
  image =
    if cfg.image == null then
      null
    else if cfg.extraImagePackages == [ ] then
      cfg.image
    else
      cfg.image.override { extraPackages = cfg.extraImagePackages; };

  # Thread the effective image through both helpers, so overriding the image
  # also changes the default image reference baked into `agent-session`.
  withImage = pkg: if image == null then pkg else pkg.override { agent-sandbox-image = image; };
in
{
  options.myconfig.ai.gvisor-agent-sandbox = with lib; {
    enable = mkEnableOption "myconfig.ai.gvisor-agent-sandbox";

    package = mkOption {
      type = types.package;
      default = pkgs.agent-session;
      defaultText = literalExpression "pkgs.agent-session";
      description = "The `agent-session` session manager package.";
    };

    image = mkOption {
      type = types.nullOr types.package;
      default = pkgs.agent-sandbox-image;
      defaultText = literalExpression "pkgs.agent-sandbox-image";
      description = ''
        Nix-built OCI image used as sandbox base. Set to `null` to manage
        images entirely outside this module (then no
        `agent-sandbox-load-image` is installed either).
      '';
    };

    extraImagePackages = mkOption {
      type = types.listOf types.package;
      default = [ ];
      example = literalExpression "[ pkgs.claude-code ]";
      description = ''
        Extra packages baked into `image` (typically the coding-agent CLI to
        run inside the sandbox). The upstream image deliberately ships no
        agent CLI; host binaries must not be bind-mounted, since that would
        drag the host `/nix` store into the sandbox.
      '';
    };

    runtime = mkOption {
      type = types.str;
      default = "runsc";
      description = "Name under which gVisor is registered as Podman OCI runtime.";
    };

    users = mkOption {
      type = types.listOf types.str;
      default = [ myconfig.user ];
      defaultText = literalExpression "[ myconfig.user ]";
      example = [ "alice" ];
      description = ''
        Users that launch sessions. They get subordinate UID/GID ranges,
        which rootless Podman requires.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ (import ./nix/overlay.nix) ];

    virtualisation.podman.enable = true;

    # Register gVisor as an alternate OCI runtime. `agent-session` selects it
    # explicitly, so ordinary Podman containers keep Podman's default runtime.
    virtualisation.containers.containersConf.settings.engine.runtimes.${cfg.runtime} = [
      "${pkgs.gvisor}/bin/runsc"
    ];

    home-manager.sharedModules = [
      {
        home.packages = [
          (withImage cfg.package)
          pkgs.gvisor
        ]
        ++ lib.optional (image != null) (withImage pkgs.agent-sandbox-load-image);
      }
    ];

    # Rootless Podman needs subordinate ID mappings for these users.
    users.users = lib.genAttrs cfg.users (_: {
      autoSubUidGidRange = true;
    });
  };
}

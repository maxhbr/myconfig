{ config, lib, pkgs, ... }:

let
  cfg = config.programs.agentSandboxes;

  # Thread the configured image through both helpers so that overriding
  # `image` also changes the default image of `agent-session`.
  withImage = pkg:
    if cfg.image == null then pkg
    else pkg.override { agent-sandbox-image = cfg.image; };
in
{
  options.programs.agentSandboxes = {
    enable = lib.mkEnableOption "rootless Podman + gVisor coding-agent sandboxes";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.agent-session;
      defaultText = lib.literalExpression "pkgs.agent-session";
      description = "The `agent-session` session manager package.";
    };

    image = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = pkgs.agent-sandbox-image;
      defaultText = lib.literalExpression "pkgs.agent-sandbox-image";
      description = ''
        Nix-built OCI image used as the sandbox base. Set to `null` to manage
        images entirely outside this module. Override it to add a coding-agent
        CLI, for example:

        ```nix
        programs.agentSandboxes.image =
          pkgs.agent-sandbox-image.override { extraPackages = [ pkgs.claude-code ]; };
        ```
      '';
    };

    runtime = lib.mkOption {
      type = lib.types.str;
      default = "runsc";
      description = "Name under which gVisor is registered as a Podman OCI runtime.";
    };

    users = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "alice" ];
      description = ''
        Users that will launch sessions. They get subordinate UID/GID ranges,
        which rootless Podman requires.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ (import ../nix/overlay.nix) ];

    virtualisation.podman.enable = true;

    # Register gVisor as an alternate OCI runtime. The session script chooses it
    # explicitly, so ordinary Podman containers keep Podman's default runtime.
    virtualisation.containers.containersConf.settings.engine.runtimes.${cfg.runtime} = [
      "${pkgs.gvisor}/bin/runsc"
    ];

    environment.systemPackages = [
      (withImage cfg.package)
      pkgs.gvisor
      pkgs.podman
    ] ++ lib.optional (cfg.image != null) (withImage pkgs.agent-sandbox-load-image);

    # Rootless Podman needs subordinate ID mappings for these users.
    users.users = lib.genAttrs cfg.users (_: { autoSubUidGidRange = true; });
  };
}

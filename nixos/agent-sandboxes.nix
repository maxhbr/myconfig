{ pkgs, ... }:

let
  agent-session = pkgs.writeShellApplication {
    name = "agent-session";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      findutils
      git
      gnugrep
      gnused
      podman
      util-linux
    ];
    text = builtins.readFile ../bin/agent-session;
  };
in
{
  virtualisation.podman.enable = true;

  # Register gVisor as an alternate OCI runtime. The session script chooses it
  # explicitly, so ordinary Podman containers continue to use Podman's default.
  virtualisation.containers.containersConf.settings.engine.runtimes.runsc = [
    "${pkgs.gvisor}/bin/runsc"
  ];

  environment.systemPackages = [
    pkgs.gvisor
    pkgs.podman
    agent-session
  ];

  # Add this to the actual interactive user that will run rootless Podman:
  # users.users.alice.autoSubUidGidRange = true;
}

{ lib
, writeShellApplication
, bashInteractive
, coreutils
, findutils
, git
, gnugrep
, gnused
, podman
, util-linux
, agent-sandbox-image
  # Default image reference used by `agent-session start --image`.
, defaultImage ? "${agent-sandbox-image.imageName}:${agent-sandbox-image.imageTag}"
}:

writeShellApplication {
  name = "agent-session";

  runtimeInputs = [
    bashInteractive
    coreutils
    findutils
    git
    gnugrep
    gnused
    podman
    util-linux
  ];

  runtimeEnv = {
    AGENT_SANDBOX_DEFAULT_IMAGE = defaultImage;
  };

  text = builtins.readFile ../bin/agent-session;

  meta = {
    description = "Manage rootless Podman + gVisor coding-agent worktree sessions";
    mainProgram = "agent-session";
    platforms = lib.platforms.linux;
  };
}

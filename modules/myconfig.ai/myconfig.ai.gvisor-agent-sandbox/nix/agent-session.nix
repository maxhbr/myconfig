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
, gvisor
, agent-sandbox-image
  # Default image reference used by `agent-session start --image`.
, defaultImage ? "${agent-sandbox-image.imageName}:${agent-sandbox-image.imageTag}"
  # Default Podman `--runtime` value. An absolute path works without
  # registering a named runtime in containers.conf.
, defaultRuntime ? "${gvisor}/bin/runsc"
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
    AGENT_SANDBOX_DEFAULT_RUNTIME = defaultRuntime;
  };

  text = builtins.readFile ../bin/agent-session;

  meta = {
    description = "Manage rootless Podman + gVisor coding-agent worktree sessions";
    mainProgram = "agent-session";
    platforms = lib.platforms.linux;
  };
}

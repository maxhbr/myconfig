{
  lib,
  writeShellApplication,
  bashInteractive,
  coreutils,
  findutils,
  git,
  gnugrep,
  gnused,
  podman,
  util-linux,
  gvisor,
  agent-gvisor-image,
  # Default image reference used by `agent-gvisor start --image`.
  defaultImage ? "${agent-gvisor-image.imageName}:${agent-gvisor-image.imageTag}",
  # Default Podman `--runtime` value. An absolute path works without
  # registering a named runtime in containers.conf.
  defaultRuntime ? "${gvisor}/bin/runsc",
}:

writeShellApplication {
  name = "agent-gvisor";

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
    AGENT_GVISOR_DEFAULT_IMAGE = defaultImage;
    AGENT_GVISOR_DEFAULT_RUNTIME = defaultRuntime;
  };

  text = builtins.readFile ../bin/agent-gvisor;

  meta = {
    description = "Manage rootless Podman + gVisor coding-agent worktree sessions";
    mainProgram = "agent-gvisor";
    platforms = lib.platforms.linux;
  };
}

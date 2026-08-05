{
  lib,
  writeShellApplication,
  coreutils,
  podman,
  agent-sandbox-image,
}:

writeShellApplication {
  name = "agent-sandbox-load-image";

  runtimeInputs = [
    coreutils
    podman
  ];

  text = ''
    image=${agent-sandbox-image}
    ref=${agent-sandbox-image.imageName}:${agent-sandbox-image.imageTag}

    if [ "''${1:-}" = "--force" ] || ! podman image exists "$ref"; then
      printf 'loading %s as %s\n' "$image" "$ref" >&2
      podman load --input "$image"
    else
      printf '%s already present; pass --force to reload\n' "$ref" >&2
    fi
  '';

  meta = {
    description = "Load the Nix-built agent sandbox image into Podman";
    mainProgram = "agent-sandbox-load-image";
    platforms = lib.platforms.linux;
  };
}

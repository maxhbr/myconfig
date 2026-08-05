final: prev: {
  # Rootless session manager for Podman + gVisor agent sandboxes.
  agent-session = final.callPackage ./agent-session.nix { };

  # The agent container image, built by Nix instead of a Containerfile.
  agent-sandbox-image = final.callPackage ./agent-image.nix { };

  # Helper that loads `agent-sandbox-image` into the caller's Podman store.
  agent-sandbox-load-image = final.callPackage ./load-image.nix { };
}

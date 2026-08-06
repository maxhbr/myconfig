final: prev: {
  # Rootless session manager for Podman + gVisor agent sandboxes.
  agent-session = final.callPackage ./agent-session.nix { };

  # The agent container image, built by Nix instead of a Containerfile.
  agent-sandbox-image = final.callPackage ./agent-image.nix { };

  # Helper that loads `agent-sandbox-image` into the caller's Podman store.
  agent-sandbox-load-image = final.callPackage ./load-image.nix { };

  # gVisor with the point-to-point address fix. Without it `runsc start` aborts
  # with EADDRNOTAVAIL on any host whose sandbox netns carries a tun-style
  # address (`10.0.0.2 peer 10.0.0.1/32`) — which is every host running OpenVPN
  # or WireGuard, because pasta names its tap after the host's default-route
  # interface and copies that interface's addresses into the sandbox netns.
  #
  # Patching is what lets the sandboxes keep runsc's default `--network=sandbox`,
  # i.e. gVisor's own userspace netstack. The alternative workaround,
  # `--network=host`, would make the untrusted workload's sockets go straight to
  # the host kernel's TCP/IP stack — a large attack surface that gVisor exists to
  # remove. See ../docs/debug-runsc-tun0-netns.md.
  gvisor = prev.gvisor.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./patches/gvisor-remove-p2p-addresses.patch ];
  });
}

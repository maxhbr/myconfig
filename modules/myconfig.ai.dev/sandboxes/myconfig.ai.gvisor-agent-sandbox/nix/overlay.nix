final: prev: {
  # Rootless session manager for Podman + gVisor agent sandboxes.
  agent-gvisor = final.callPackage ./agent-gvisor.nix { };

  # The agent container image, built by Nix instead of a Containerfile.
  agent-gvisor-image = final.callPackage ./agent-image.nix { };

  # Helper that loads `agent-gvisor-image` into the caller's Podman store.
  agent-gvisor-load-image = final.callPackage ./load-image.nix { };

  # gVisor, bumped ahead of nixpkgs (which still ships 20260406.0) to the
  # release-20260817.0 content on the synthetic `go` branch, plus the
  # point-to-point address fix.
  #
  # Why the bump: the nixpkgs pin (20260406.0, `db8d2c9a`) predates upstream
  # `cfb7c0629521` (2026-06-05, "Fix SIGWINCH delivery on PTY window size change
  # (TIOCSWINSZ)", fixes google/gvisor#13317). Without it, no program inside a
  # sandbox ever receives SIGWINCH when a pty is resized, so TUIs (herdr panes,
  # pi, shells, editors) keep rendering at the old width after a host terminal
  # (foot) resize — the pty size itself (`TIOCGWINSZ`) is already correct, only
  # the signal is missing. See doc/TODOs/drop-gvisor-version-override-sigwinch.md
  # for dropping this override again.
  #
  # Point-to-point address fix: without it `runsc start` aborts with
  # EADDRNOTAVAIL on any host whose sandbox netns carries a tun-style address
  # (`10.0.0.2 peer 10.0.0.1/32`) — which is every host running OpenVPN or
  # WireGuard, because pasta names its tap after the host's default-route
  # interface and copies that interface's addresses into the sandbox netns.
  # Patching is what lets the sandboxes keep runsc's default `--network=sandbox`,
  # i.e. gVisor's own userspace netstack. The alternative workaround,
  # `--network=host`, would make the untrusted workload's sockets go straight to
  # the host kernel's TCP/IP stack — a large attack surface that gVisor exists to
  # remove. See ../docs/debug-runsc-tun0-netns.md.
  #
  # "Merge release-20260817.0-38-ged1b001b8 (automated)" is the last automated
  # merge of release-20260817.0 into the `go` branch, following the nixpkgs
  # convention of pinning such merge commits (buildable without bazel).
  #
  # `patches` is reset (not extended): the inherited nixpkgs
  # `fix-go-mod-tidy.diff` targets the 20260406.0 go.mod and no longer applies
  # (rules_go is gone from the module graph; the other removed entries moved or
  # became direct deps). Only the p2p patch is kept.
  gvisor =
    (prev.gvisor.overrideAttrs (old: {
      version = "20260817.0";
      src = prev.fetchFromGitHub {
        owner = "google";
        repo = "gvisor";
        rev = "b1b561450fc2f05b9626b7e269c08fbc9f5029ff";
        hash = "sha256-wPivgyA8tRo5yn/U7W/aLGMzOA8EOP8TVo2Sk1k+02U=";
      };
      vendorHash = "sha256-aUoxxXbADItHBjkyg50r087Z/NQBIIR1AtTleD3rxdY=";
      patches = [ ];
    })).overrideAttrs
      (old: {
        patches = (old.patches or [ ]) ++ [ ./patches/gvisor-remove-p2p-addresses.patch ];
      });
}

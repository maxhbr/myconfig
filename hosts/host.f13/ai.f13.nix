# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  config = {
    myconfig = {
      ai = {
        enable = true;
        opencode.enable = true;
        pi-coding-agent.enable = true;
        claude-code.enable = true;
        codex.enable = true;
        skills.enable = true;

        # Rootless Podman + gVisor agent-sandbox tier (`agent-session`); see
        # modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/README.md.
        # Like `microvm` below, it is enabled explicitly per host and never
        # implicitly through the broad `myconfig.ai.enable`.
        gvisor-agent-sandbox = {
          enable = true;
          # The upstream image deliberately ships no agent CLI, and host
          # binaries must not be bind-mounted (that would drag the host
          # /nix store into the sandbox), so bake the agents in instead.
          extraImagePackages = [
            # same attributes the host wrappers use (see
            # modules/myconfig.ai/programs.pi-coding-agent, programs.opencode)
            pkgs.nixos-unstable.pi-coding-agent
            pkgs.opencode
          ];
        };

        # Cloud Hypervisor agent-sandbox tier
        # (see modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm.md).
        # Explicitly enabled here — NOT via the broad `myconfig.ai.enable`
        # — because it is a much stronger (and more resource-heavy) isolation
        # tier that must never switch on implicitly with the other AI tools.
        # The secure `proxy-only` network profile applies, so no
        # `acknowledgeInsecureNetwork` opt-in is needed.
        microvm = {
          enable = true;
          # Fixed, prebuilt resource classes (ticket 5). `mkForce` defines the
          # pool EXHAUSTIVELY — a plain definition would merge with the module's
          # default `normal` class. Reduced to a single slot per class for
          # testing on this laptop.
          resourceClasses = lib.mkForce {
            small = {
              count = 1;
              vcpu = 2;
              memoryMiB = 4096;
            };
            normal = {
              count = 1;
              vcpu = 4;
              memoryMiB = 8192;
            };
          };
          # Named network profile (the secure default): the guest's only egress
          # is the bridge-only host LiteLLM endpoint. Guest-to-guest traffic,
          # the host LAN/VPN, cloud metadata and the public internet are all
          # blocked. Replaces the old allowPublicInternet /
          # allowPrivateNetworks / allowInterVmTraffic booleans.
          networkProfile = "proxy-only";
          # Operator convenience on this interactive laptop: mhuber (already a
          # full sudoer) drives `agent-microvm` / the workmux microvm-* panes
          # without a password prompt. Does not affect guest isolation — the
          # untrusted guest can never reach host sudo (see module option doc).
          passwordlessControl = true;
          # Dedicated public key authorising ONLY the guest `agent` user —
          # never the host, and never a host authorized_keys file. A public
          # key is not a secret, so it is committed in-repo. The MATCHING
          # PRIVATE key is NOT in this repo: it is managed out-of-band and
          # lives in the separate ../priv repository. Point ssh at it via
          # `AGENT_MICROVM_SSH_KEY=/path/to/private-key agent-microvm ssh …`.
          sshPublicKeyFile = ./dedicated-agent-vm-key.pub;
        };
      };
    };

    home-manager.sharedModules = [
      {
        home.packages =
          let
            ai-tmux-session = "ai";
            ai-tmux-session-script = pkgs.writeShellScriptBin "ai-tmux-session" ''
              # if session is not yet created, create it
              if ! tmux has-session -t ${ai-tmux-session}; then
                tmux new-session -d -s ${ai-tmux-session}
                tmux send-keys -t ${ai-tmux-session}:1 "btop" C-m
                tmux split-window -h -t ${ai-tmux-session}
                tmux send-keys -t ${ai-tmux-session}:1 "nvtop -i" C-m
                tmux split-window -v -t ${ai-tmux-session}
                tmux send-keys -t ${ai-tmux-session}:1 "journalctl -f" C-m
                tmux split-window -v -t ${ai-tmux-session}
              fi
              exec tmux attach-session -t ${ai-tmux-session}
            '';
          in
          [ ai-tmux-session-script ];
      }
    ];
  };
}

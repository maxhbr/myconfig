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

        # Cloud Hypervisor agent-sandbox tier (see docs/agent-microvm.md).
        # Explicitly enabled here — NOT via the broad `myconfig.ai.enable`
        # — because it is a much stronger (and more resource-heavy) isolation
        # tier that must never switch on implicitly with the other AI tools.
        # All insecure network relaxations stay false, so the secure
        # proxy-only default applies and no `acknowledgeInsecureNetwork` is
        # needed.
        microvm = {
          enable = true;
          slotCount = 4;
          defaultVcpu = 4;
          defaultMemoryMiB = 8192;
          allowPublicInternet = false;
          allowPrivateNetworks = false;
          allowInterVmTraffic = false;
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

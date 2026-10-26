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
        llmops.enable = true;
        dev = {
          enable = true;
          mysbx.config = {
            memory = lib.mkDefault "8g";
            cpus = lib.mkDefault 5;
          };
          # The heavy backends this host runs (bd myconfig-dq2w): f13 is
          # the krun validation host (docs/krun-live-validation.md, the
          # fd-stress runbooks of bd myconfig-jjfy/dgsm) and carries the
          # podman backends for podman-krun. Off by default everywhere
          # else — the flags gate the closure inputs.
          mysbx.podman.enable = true;
          mysbx.krun.enable = true;
          mysbx.microvm-qemu.enable = true;

          # microvm = {
          #   enable = false;
          #   enabledAgents = [
          #     "claude"
          #     "codex"
          #     "herdr"
          #     "hermes"
          #     "opencode"
          #     "pi"
          #   ];
          #   resourceClasses = lib.mkForce {
          #     small = {
          #       count = 1;
          #       vcpu = 2;
          #       memoryMiB = 4096;
          #     };
          #     normal = {
          #       count = 1;
          #       vcpu = 4;
          #       memoryMiB = 8192;
          #     };
          #   };
          #   workspaceLayout = "beside-repo";
          #   networkProfile = "proxy-only";
          #   passwordlessControl = true;
          #   sshPublicKeyFile = ./dedicated-agent-vm-key.pub;
          #   guestShellConvenience.enable = true;
          # };
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

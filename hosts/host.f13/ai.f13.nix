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
          mysbx = {
            config = {
              memory = lib.mkDefault "8g";
              cpus = lib.mkDefault 5;
            };
            # podman.enable = true;
            krun.enable = true;
            vm.store.enable = true;
            # The assistant flavor (bd myconfig-xl93): a `mysbx-assistant`
            # launcher with himalaya on the sandbox PATH, for agents that
            # read the Maildir a repo sidecar mounts read-only.
            profiles.assistant.extraTools = [
              pkgs.himalaya
              pkgs.dstask
            ];
            microvm-qemu.enable = true;
            microvm-ch.enable = true;
          };

          # agent-of-empires (`aoe`) is enabled by the `myconfig.ai.dev`
          # umbrella default; disable it here (see
          # modules/myconfig.ai.dev/default.nix).
          agent-of-empires.enable = lib.mkForce false;

          github-copilot-cli.enable = true;

          thinkrail.enable = true;

          defaultHugeModel = "GLM-5.3";
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

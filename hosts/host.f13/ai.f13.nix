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
        # The sandbox image gets the agent CLIs enabled above
        # (pi-coding-agent, opencode, claude-code, codex) baked in
        # automatically; see `extraImagePackages` in the module.
        gvisor-agent-sandbox.enable = true;

        # Cloud Hypervisor agent-sandbox tier
        # (see modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm.md).
        # Explicitly enabled here — NOT via the broad `myconfig.ai.enable`
        # — because it is a much stronger (and more resource-heavy) isolation
        # tier that must never switch on implicitly with the other AI tools.
        # The secure `proxy-only` network profile applies, so no
        # `acknowledgeInsecureNetwork` opt-in is needed.
        #
        # The module has exactly ONE shape (the lightweight one of
        # docs/myconfig-ai-microvm-lightweight-plan.md): a minimal guest
        # toolset with a bash login shell, an EROFS/optimized guest store, ONE
        # writable per-session virtiofs share plus ONE read-only share, and a
        # guest home provisioned at LAUNCH time from an allowlisted, root-owned
        # staged copy of this host's agent configuration (no guest
        # home-manager). Only the two options below deviate from its defaults.
        microvm = {
          enable = true;
          # SELECTED agents (lightweight plan phase 2). Stated EXPLICITLY even
          # though it currently equals the module default (every declared
          # agent), because the selection is operator-visible: `myconfig.ai
          # .workmux` registers one `microvm-<agent>` pane per selected agent
          # and `agent-microvm submit|run --agent <name>` only accepts a
          # selected one — so silently inheriting a narrower default would
          # remove panes and reject invocations that work today. The cost is the
          # guest closure: EVERY listed agent's runtime is baked into BOTH slot
          # images. Trimming this list is the cheapest way to shrink the guest,
          # and it is deliberately an operator decision, not a default.
          # `herdr` is the agent MULTIPLEXER (../../modules/myconfig.ai/
          # programs.herdr.nix). Selecting it bakes `pkgs.herdr` into the guest
          # closure and registers a `microvm-herdr` workmux pane, so a guest can
          # run `herdr` (via `agent-microvm run --agent herdr` / the pane) and,
          # from inside its TUI, launch the OTHER agents selected below (they
          # are on the guest PATH) — mirroring the tier-3 `sandboxed-herdr`
          # variant. It has no batch mode, so `submit --agent herdr` is rejected.
          enabledAgents = [
            "claude"
            "codex"
            "herdr"
            "hermes"
            "opencode"
            "pi"
          ];
          # Fixed, prebuilt resource classes (ticket 5). `mkForce` defines the
          # pool EXHAUSTIVELY — a plain definition would merge with the module's
          # default `normal` class. Reduced to a single slot per class for
          # testing on this laptop. An explicit table always outranks whatever
          # the module defaults to.
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

          # OPT-IN shell convenience (modules/myconfig.ai/myconfig.ai.microvm/
          # guest-shell-convenience.nix): bake fish + neovim (built from the
          # GUEST's own pkgs, so they land in the immutable EROFS store) into
          # every guest closure and render the host's *config source* for them
          # into the disposable home. An operator who SSHes into a sandbox
          # gets the same fish prompt/abbreviations and neovim keybindings as
          # on the host, WITHOUT running home-manager in the guest or staging
          # the host's rendered (host-closure-coupled) dotfiles. Cost: a
          # sizeable addition to the guest closure; OFF by default, enabled
          # here because this laptop's operator drives sandboxes interactively.
          guestShellConvenience.enable = true;
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

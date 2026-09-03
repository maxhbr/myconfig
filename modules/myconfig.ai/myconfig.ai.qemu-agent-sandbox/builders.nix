# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Sandboxed microVM runners — the microvm.nix counterparts of the bubblewrap
# `agent-bubblewrap-*` wrappers. The agent (or the whole workmux/tmux session) runs
# inside a real VM (its own kernel) with an ephemeral, discarded-on-exit root
# filesystem and an unprivileged `agent` user, instead of a bubblewrap jail.
#
# Exports:
#   * `mkSandboxedRunner`         — generic parameterized qemu runner builder.
#     * `mkSeedScript`         — builds the `seed-agent-config` host-side
#                                 seeder for a given config-path allowlist
#                                 (see modules/myconfig.ai/fns/seed-agent-config.nix).
#   * `mkAgentQemuPiRunner`       — thin wrapper: one workspace + `pi`.
#                                   Backs `agent-qemu-pi` (agent-bubblewrap-pi analogue).
#   * `mkAgentQemuHerdrRunner`    — thin wrapper: one workspace + `herdr` and
#                                   the coding-agent CLIs it launches. Backs
#                                   `agent-qemu-herdr` (herdr-in-VM variant of
#                                   `agent-qemu-pi`).
#   * `mkSandboxedWorkmuxRunner`  — main repo + its `__worktrees` sibling +
#                                   tmux/workmux/pi. Backs
#                                   `agent-qemu-alacritty-workmux-tmux`
#                                   (agent-bubblewrap-alacritty-workmux-tmux analogue).
#
# The host-side wrappers (see
# `modules/myconfig.ai/programs.pi-coding-agent/default.nix` and
# `modules/myconfig.ai/myconfig.ai.workmux/sandbox.nix`) evaluate these per
# invocation via a standalone impure Nix expression, passing the current working directory
# as the workspace. This is the "wrapper that evaluates a parameterized flake
# output" execution model — the guest system closure is cached; only a tiny
# wrapper derivation that embeds the workspace path and the forwarded SSH port
# rebuilds each launch (sub-second).
#
# ── Why qemu + user-mode networking (not cloud-hypervisor) ──────────────────
# cloud-hypervisor only supports `tap`/`macvtap` interfaces (see
# microvm.nix/lib/runners/cloud-hypervisor.nix) — it has no user-mode
# networking and no `forwardPorts`. Using it would require a host bridge,
# per-VM TAP, NAT and firewall rules plus a system rebuild before the guest
# could reach the network or be reachable over SSH. That defeats the
# `agent-bubblewrap-*`-like goal of a self-contained user-space wrapper that "just
# works" from any project directory with no host changes. qemu supports
# `type = "user"` SLiRP networking + `forwardPorts` (the same combination the
# in-repo hermes microVM uses), giving outbound NAT and a host-localhost SSH
# port with zero host configuration. cloud-hypervisor + a dedicated bridge is
# the natural follow-up once host networking is provisioned; only
# `microvm.hypervisor`/`interfaces`/`forwardPorts` differ.
#
# ── Security posture (vs the bubblewrap jails) ──────────────────────────────
# Strictly stronger isolation than the bubblewrap jail: the agent runs in a
# separate kernel as an unprivileged `agent` user with an ephemeral,
# discarded-on-exit root filesystem. Only the explicitly listed host
# directories are shared read-write. The host home directory, credentials,
# agent/ssh sockets, D-Bus, the nix daemon and the host /nix/store (shared
# read-only only) are never writable from the guest. LLM credentials are
# forwarded at launch over the SSH channel's environment (never baked into the
# Nix store, never in process argv).
#
# ── Agent-configuration seeding ────────────────────────────────────────────
# The guest home is an ephemeral tmpfs, so an agent would otherwise start with
# empty/default configuration. To avoid that, the host wrapper copies the
# RELEVANT, ALLOWLISTED host configuration into the guest `/home/agent` over
# the SSH channel at launch (after the VM boots, before the agent is exec'd) —
# see `mkSeedScript` below and
# modules/myconfig.ai/fns/seed-agent-config.nix. Only non-sensitive
# configuration (settings, extensions, themes, skills) is copied; credential
# files are excluded by a denylist applied to every path component AND the
# resolved target, so secrets never touch the Nix store (they keep flowing
# over the SSH environment, exactly as before). The allowlist + denylist are
# BAKED into the seeder script by Nix; the launcher cannot widen them.
{
  nixpkgs,
  nixosSystem,
  microvmModule,
  seedAgentConfig,
}:
let
  # The shared host→guest agent-config seeder library
  # (modules/myconfig.ai/fns/seed-agent-config.nix). Imported once so every
  # runner factory shares the same allowlist/denylist vocabulary. Resolved
  # against x86_64-linux (the only sandboxed-* host platform); the library is
  # platform-independent (it only uses lib + writeShellApplication).
  seedLib = import seedAgentConfig {
    inherit (nixpkgs) lib;
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  };

  # Build the `seed-agent-config` host-side seeder script for a given union
  # config-path allowlist. The resulting script is added to the runner's
  # `bin/` so the host wrapper can invoke it as
  #   $runner/bin/seed-agent-config <ssh-port> <identity> <host> <user>
  # after the VM boots. The allowlist + denylist are baked in; the script takes
  # no policy argument. See modules/myconfig.ai/fns/seed-agent-config.nix.
  mkSeedScript =
    { system, configPaths }:
    if configPaths == [ ] then
      # No seeding requested: emit a no-op so the wrapper can call it
      # unconditionally. Keeps the wrapper logic branch-free.
      nixpkgs.legacyPackages.${system}.writeShellScriptBin "seed-agent-config" ''
        echo "seed-agent-config: nothing to seed (empty allowlist)" >&2
      ''
    else
      seedLib.mkSeeder { inherit configPaths; };

  # Generic sandbox guest/runner builder shared by all sandboxed-* wrappers.
  #
  # Arguments:
  #   system              — e.g. "x86_64-linux".
  #   hostname            — guest hostname.
  #   sshPort             — host-localhost TCP port forwarded to guest :22.
  #   authorizedKeysFile  — throwaway SSH public key authorized for `agent`.
  #   shares              — list of extra writable virtiofs shares
  #                         ({ tag; source; mountPoint; }). The read-only
  #                         host /nix/store share is always added.
  #   guestPackages       — extra packages on the guest system PATH.
  #   extraGuestModules   — extra NixOS modules merged into the guest.
  #   vcpu / mem          — guest resource bounds.
  #   allowNetwork        — when false, no network interface / SSH (offline).
  #   hostUid / hostGid   — when non-null, pin the unprivileged guest `agent`
  #                         user's uid/gid to these values (the invoking host
  #                         user's own uid/gid). virtiofsd runs unprivileged
  #                         (`--sandbox none`, no `--translate-uid`), so every
  #                         share is passed through with the REAL host
  #                         owner/mode unchanged; the guest kernel then
  #                         enforces the usual POSIX permission check against
  #                         the calling (guest) process's uid/gid. Without a
  #                         match, `agent`'s auto-assigned guest uid is only
  #                         accidentally the same as the host owner, so
  #                         writes to an owner-only-writable share can fail
  #                         with EACCES even though the world-readable bits
  #                         still make reads succeed. Defaults to null (no
  #                         pinning), preserving the previous behaviour.
  mkSandboxedRunner =
    {
      system,
      hostname,
      sshPort,
      authorizedKeysFile,
      shares ? [ ],
      guestPackages ? [ ],
      extraGuestModules ? [ ],
      vcpu ? 4,
      mem ? 8192,
      allowNetwork ? true,
      hostUid ? null,
      hostGid ? null,
      # The host→guest config-seed allowlist (a union of per-agent
      # `configPaths`, see modules/myconfig.ai/fns/seed-agent-config.nix).
      # When non-empty, a `bin/seed-agent-config` seeder script is built into
      # the runner output so the host wrapper can copy the allowlisted host
      # configuration into the guest home over SSH at launch. When empty
      # (the default), no seeding happens and the wrapper starts the agent
      # with an empty/default config (the previous behaviour).
      seedConfigPaths ? [ ],

      # Shared sandbox tooling (see
      # modules/myconfig.ai/myconfig.ai.sandboxTools.nix): store-path strings
      # baked into a `SANDBOXED_*_EXTRA_PACKAGES` JSON env var by the host-side
      # wrapper (same pattern as `AGENT_QEMU_HERDR_AGENT_PACKAGES`) and passed
      # through by `runner.nix`. Folded into ONE
      # `buildEnv` package that joins `guestPackages`.
      extraGuestPackagePaths ? [ ],
    }:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      guest = nixosSystem {
        inherit system;
        modules = [
          microvmModule
          (
            { lib, ... }:
            {
              networking.hostName = hostname;

              microvm = {
                hypervisor = "qemu";
                inherit vcpu mem;
                graphics.enable = false;

                # Read-only host /nix/store share (no store disk image needed,
                # fast boot) + the caller's writable shares.
                shares = [
                  {
                    tag = "nix-store";
                    source = "/nix/store";
                    mountPoint = "/nix/store";
                    proto = "virtiofs";
                    readOnly = true;
                  }
                ]
                ++ map (s: s // { proto = "virtiofs"; }) shares;

                # SLiRP user-mode networking: outbound NAT through the host,
                # no host bridge/tap, no inbound except the forwarded SSH port.
                interfaces = lib.optionals allowNetwork [
                  {
                    type = "user";
                    id = "qemu";
                    mac = "02:00:00:5a:9d:01";
                  }
                ];
                forwardPorts = lib.optionals allowNetwork [
                  {
                    from = "host";
                    host.address = "127.0.0.1";
                    host.port = sshPort;
                    guest.port = 22;
                  }
                ];
              };

              # Ephemeral root: no `microvm.volumes`, so the root filesystem is
              # a tmpfs that is discarded when the VM stops. Only the shares
              # (and the read-only host store) survive.

              # ── Unprivileged agent user ─────────────────────────────────
              # When `hostUid`/`hostGid` are given, `agent` is pinned to the
              # invoking host user's own uid/gid so the ownership virtiofsd
              # passes through unchanged (no `--translate-uid`) matches the
              # guest-side permission check exactly, keeping every share
              # writable exactly as it is on the host — see `mkSandboxedRunner`
              # above for the failure mode this avoids.
              users.users.agent = {
                isNormalUser = true;
                home = "/home/agent";
                createHome = true;
                extraGroups = [ ];
                # No password; access is key-only over SSH (or console).
                hashedPassword = "!";
                shell = pkgs.bashInteractive;
                openssh.authorizedKeys.keyFiles = [ authorizedKeysFile ];
              }
              // lib.optionalAttrs (hostUid != null) {
                uid = hostUid;
                group = "agent";
              };

              users.groups = lib.optionalAttrs (hostGid != null) {
                agent.gid = hostGid;
              };

              security.sudo.enable = false;

              # ── SSH: key-only, no root, no forwarding ───────────────────
              services.openssh = lib.mkIf allowNetwork {
                enable = true;
                settings = {
                  PermitRootLogin = "no";
                  PasswordAuthentication = false;
                  KbdInteractiveAuthentication = false;
                  AllowAgentForwarding = "no";
                  X11Forwarding = false;
                  AllowTcpForwarding = "no";
                  PermitTunnel = "no";
                  # Allow the launcher to forward LLM credentials at runtime
                  # via the SSH environment (never baked into the store).
                  AcceptEnv = [
                    "OPENAI_API_KEY"
                    "OPENAI_BASE_URL"
                    "OPENROUTER_BASE_URL"
                    "ANTHROPIC_API_KEY"
                    "PI_*"
                    "LANG"
                    "LC_ALL"
                    "TERM"
                    "COLORTERM"
                  ];
                };
              };
              networking.firewall.allowedTCPPorts = lib.optionals allowNetwork [ 22 ];

              # ── Minimal coding-agent environment ────────────────────────
              environment.systemPackages =
                (with pkgs; [
                  bashInteractive
                  coreutils
                  curl
                  diffutils
                  fd
                  file
                  findutils
                  git
                  gnugrep
                  gnumake
                  gnused
                  jq
                  less
                  openssh
                  patch
                  procps
                  ripgrep
                  rsync
                  tree
                  unzip
                  which
                ])
                ++ guestPackages
                ++ lib.optional (extraGuestPackagePaths != [ ]) (
                  pkgs.buildEnv {
                    name = "sandboxed-extra-tools";
                    paths = extraGuestPackagePaths;
                  }
                );

              system.stateVersion = "25.11";
            }
          )
        ]
        ++ extraGuestModules;
      };

      lib = nixpkgs.lib;

      # microvm.nix's declared qemu runner. Its `bin/microvm-run` launches
      # qemu, which connects to the virtiofs daemons over RELATIVE unix
      # socket paths (`<hostname>-virtiofs-<tag>.sock`) in its working
      # directory.
      runner = guest.config.microvm.declaredRunner;

      # All virtiofs shares this guest exposes (the read-only host
      # /nix/store plus the caller's writable shares). microvm.nix names
      # each control socket `<hostname>-virtiofs-<tag>.sock` (see
      # nixos-modules/microvm/options.nix); qemu uses exactly these names.
      virtiofsShares = builtins.filter (s: (s.proto or "virtiofs") == "virtiofs") (
        guest.config.microvm.shares
      );

      virtiofsdPkg = pkgs.virtiofsd;

      # Rootless virtiofsd launch line per share. microvm.nix's own
      # `virtiofsd-run` drives these daemons through a supervisord config
      # hard-coded with `user=root`, so it only works from the root-run
      # `microvm-virtiofsd@` systemd unit — it aborts with "Can't drop
      # privilege as nonroot user" when a normal user runs it. The
      # sandboxed-* wrappers deliberately run entirely in user space (no
      # host rebuild, no systemd unit, no sudo), so we start virtiofsd
      # ourselves with `--sandbox none` (no daemon-side namespace, which
      # would need privileges we don't have) instead. virtiofsd runs as the
      # invoking user and only re-exports directories that user can already
      # read/write, so this grants the VM no access the user lacks; the
      # isolation boundary is the guest kernel, exactly as before.
      virtiofsdLine =
        share:
        let
          socket = "${guest.config.networking.hostName}-virtiofs-${share.tag}.sock";
        in
        ''
          ${lib.getExe virtiofsdPkg} \
            --socket-path=${lib.escapeShellArg socket} \
            --shared-dir=${lib.escapeShellArg (toString share.source)} \
            --sandbox none \
            --cache=auto \
            ${lib.optionalString (share.readOnly or false) "--readonly"} \
            >>"$PWD/virtiofsd.log" 2>&1 &
          virtiofsd_pids+=("$!")
        '';

      socketFor = share: "${guest.config.networking.hostName}-virtiofs-${share.tag}.sock";

      # Combined launcher: start the rootless virtiofsd daemon(s), wait for
      # their sockets to appear, then run qemu — all from the SAME working
      # directory (the caller cd's into a per-invocation runtime_dir), since
      # the socket paths are relative. virtiofsd is torn down together with
      # the VM via the trap.
      launcher = pkgs.writeShellApplication {
        name = "sandboxed-launch";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
          virtiofsd_pids=()
          vm_pid=""
          cleanup() {
            if [ -n "$vm_pid" ] && kill -0 "$vm_pid" 2>/dev/null; then
              kill "$vm_pid" 2>/dev/null || true
              wait "$vm_pid" 2>/dev/null || true
            fi
            for p in "''${virtiofsd_pids[@]:-}"; do
              [ -n "$p" ] && kill "$p" 2>/dev/null || true
            done
          }
          trap cleanup EXIT INT TERM

          ${lib.concatMapStrings virtiofsdLine virtiofsShares}

          # Wait for every virtiofs socket to be created before starting qemu
          # (qemu exits immediately if it cannot connect to a share socket).
          for sock in ${lib.escapeShellArgs (map socketFor virtiofsShares)}; do
            ready=0
            for _ in $(seq 1 100); do
              if [ -S "$sock" ]; then
                ready=1
                break
              fi
              sleep 0.1
            done
            if [ "$ready" -ne 1 ]; then
              echo "sandboxed-launch: timed out waiting for virtiofs socket: $sock" >&2
              cat "$PWD/virtiofsd.log" >&2 || true
              exit 1
            fi
          done

          ${lib.getExe' runner "microvm-run"} &
          vm_pid=$!
          wait "$vm_pid"
        '';
      };
    in
    # A thin package exposing microvm.nix's original scripts (so
    # `bin/microvm-run`, `bin/microvm-shutdown`, ... stay available), the
    # combined `bin/sandboxed-launch` entry point the host wrappers call, and
    # — when `seedConfigPaths` is non-empty — the `bin/seed-agent-config`
    # seeder the wrapper runs after the VM boots to copy the allowlisted host
    # configuration into the guest home over SSH.
    pkgs.symlinkJoin {
      name = "sandboxed-runner-${guest.config.networking.hostName}";
      paths = [
        launcher
        runner
      ]
      ++ lib.optional (seedConfigPaths != [ ]) (mkSeedScript {
        inherit system;
        configPaths = seedConfigPaths;
      });
    };

in
{
  inherit mkSandboxedRunner;

  # One workspace + `pi`. Backs `agent-qemu-pi`.
  mkAgentQemuPiRunner =
    {
      system,
      workspace,
      sshPort,
      authorizedKeysFile,
      piPackage,
      vcpu ? 4,
      mem ? 8192,
      allowNetwork ? true,
      # Override the default `pi`-only seed allowlist. Defaults to the `pi`
      # agent's configPaths from the shared seeder library.
      seedConfigPaths ? seedLib.configPathsFor [ "pi" ],
      # Shared sandbox tooling store paths (see mkSandboxedRunner).
      extraGuestPackagePaths ? [ ],
    }:
    mkSandboxedRunner {
      inherit
        system
        sshPort
        authorizedKeysFile
        vcpu
        mem
        allowNetwork
        seedConfigPaths
        ;
      hostname = "agent-qemu-pi";
      shares = [
        {
          tag = "workspace";
          source = workspace;
          mountPoint = "/workspace";
        }
      ];
      guestPackages = [ piPackage ];
      inherit extraGuestPackagePaths;
    };

  # One workspace + `herdr` and the coding-agent CLIs it launches. Backs
  # `agent-qemu-herdr`. Unlike `mkAgentQemuPiRunner` (which carries just `pi`),
  # the guest gets `herdr` (the agent multiplexer the user is dropped into)
  # plus whichever coding-agent CLIs `herdr` is expected to start from inside
  # the VM — the same set the gVisor sandbox image bakes in (see
  # modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/default.nix,
  # `agentPackagesByFlag`). The host wrapper execs `herdr` (not `pi`) over SSH;
  # from within that `herdr` session the user starts `pi` / `opencode` / etc.
  mkAgentQemuHerdrRunner =
    {
      system,
      workspace,
      sshPort,
      authorizedKeysFile,
      herdrPackage,
      agentPackages ? [ ],
      vcpu ? 4,
      mem ? 8192,
      allowNetwork ? true,
      # Pin the guest `agent` user's uid/gid to the invoking host user's own
      # uid/gid (see `mkSandboxedRunner` above). Passed through by the
      # `agent-qemu-herdr` host wrapper as `AGENT_QEMU_HERDR_UID`/`_GID` (`id
      # -u`/`id -g`), so writes to the shared workspace succeed regardless of
      # which uid the guest's ephemeral user database happens to allocate.
      hostUid ? null,
      hostGid ? null,
      # Override the default seed allowlist. Defaults to the union of
      # `configPaths` for every registered agent (pi, opencode, claude, codex,
      # qwen-code, github-copilot-cli, hermes) plus `herdr` itself (the guest
      # entry point, whose rendered keybinding config is staged too), so a
      # `agent-qemu-herdr` guest is seeded with the configuration for ALL
      # agents `herdr` can launch. The union follows the shared seeder
      # library's `configPathsFor`.
      seedConfigPaths ? seedLib.configPathsFor [
        "herdr"
        "pi"
        "opencode"
        "claude"
        "codex"
        "qwen-code"
        "github-copilot-cli"
        "hermes"
      ],
      # Shared sandbox tooling store paths (see mkSandboxedRunner).
      extraGuestPackagePaths ? [ ],
    }:
    mkSandboxedRunner {
      inherit
        system
        sshPort
        authorizedKeysFile
        vcpu
        mem
        allowNetwork
        seedConfigPaths
        hostUid
        hostGid
        ;
      hostname = "agent-qemu-herdr";
      shares = [
        {
          tag = "workspace";
          source = workspace;
          mountPoint = "/workspace";
        }
      ];
      guestPackages = [ herdrPackage ] ++ agentPackages;
      inherit extraGuestPackagePaths;
    };

  # Main repo + its `__worktrees` sibling + tmux/workmux/pi. Backs
  # `agent-qemu-alacritty-workmux-tmux`.
  #
  # The main checkout is shared read-write at /workspace and the sibling
  # worktrees directory at /workspace__worktrees, so that inside the guest
  # workmux's `dirname(top)/basename(top)__worktrees` convention resolves
  # correctly (dirname "/workspace" = "/", basename = "workspace" →
  # "/workspace__worktrees"). The guest entry point `workmux-sandbox-entry`
  # installs the supplied workmux config, boots a workmux tmux session and
  # attaches; the launcher SSHes in and runs it under a TTY.
  mkSandboxedWorkmuxRunner =
    {
      system,
      workspace,
      worktrees,
      sshPort,
      authorizedKeysFile,
      piPackage,
      workmuxPackage,
      workmuxConfigFile,
      tmuxConf ? "",
      vcpu ? 4,
      mem ? 8192,
      allowNetwork ? true,
      # Shared sandbox tooling store paths (see mkSandboxedRunner).
      extraGuestPackagePaths ? [ ],
    }:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      # Guest entry point: install the jail-specific workmux config, boot a
      # workmux tmux session (mirrors the bubblewrap jail's entry in
      # myconfig.ai.workmux/jail.nix) and attach. Runs in /workspace so
      # workmux operates on the shared main checkout.
      entry = pkgs.writeShellApplication {
        name = "workmux-sandbox-entry";
        runtimeInputs = [
          workmuxPackage
          pkgs.tmux
          pkgs.coreutils
          pkgs.bashInteractive
          pkgs.git
          piPackage
        ];
        text = ''
          # Install the sandbox workmux config (plain `pi` agent, not the
          # bubblewrap-wrapped one — the VM is already the sandbox).
          mkdir -p ~/.config/workmux
          install -m 0644 ${workmuxConfigFile} ~/.config/workmux/config.yaml

          cd /workspace

          session=workmux
          shell=${pkgs.lib.getExe pkgs.bashInteractive}
          export SHELL="$shell"

          if ! tmux has-session -t "=$session" 2>/dev/null; then
            tmux \
              set-option -g default-shell "$shell" \; \
              set-option -g default-command "$shell" \; \
              new-session -d -s "$session" -c /workspace
          fi

          if [ "$(tmux show-options -t "=$session:" -qv @workmux_bootstrapped)" != 1 ]; then
            tmux set-option -t "=$session:" @workmux_bootstrapped 1
            tmux send-keys -t "=$session:" 'workmux sidebar --session; workmux dashboard' Enter
          fi

          exec tmux attach-session -t "=$session"
        '';
      };
    in
    mkSandboxedRunner {
      inherit
        system
        sshPort
        authorizedKeysFile
        vcpu
        mem
        allowNetwork
        ;
      hostname = "agent-qemu-workmux-tmux";
      shares = [
        {
          tag = "workspace";
          source = workspace;
          mountPoint = "/workspace";
        }
        {
          tag = "worktrees";
          source = worktrees;
          mountPoint = "/workspace__worktrees";
        }
      ];
      guestPackages = [
        piPackage
        workmuxPackage
        pkgs.tmux
        entry
      ];
      inherit extraGuestPackagePaths;
      # Expose the host's tmux configuration inside the guest so the in-VM
      # tmux server picks up the same keybindings/theme. Skipped when empty.
      extraGuestModules = pkgs.lib.optional (tmuxConf != "") {
        environment.etc."tmux.conf".source = tmuxConf;
      };
    };
}

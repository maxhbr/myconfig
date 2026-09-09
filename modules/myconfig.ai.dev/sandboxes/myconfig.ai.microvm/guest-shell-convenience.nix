# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — OPT-IN guest shell convenience (fish + neovim).
#
# This is the mechanism recommended in place of "run home-manager in the
# guest" or "stage the host primary user's rendered dotfiles": it bakes a
# small, GUEST-BUILT toolset (fish, neovim) into the immutable guest closure
# and renders the host's *config source* for them from the guest's own pkgs
# into the disposable home.
#
# Why this shape and not home-manager / not staging the rendered profile
# ---------------------------------------------------------------------
# Phase 3 (../config-seed.nix) deliberately REMOVED the in-guest home-manager
# activation (`guest-home.nix`): every config edit needed a guest rebuild and
# the guest carried the whole activation machinery. Re-introducing it would
# re-open that hole. Staging the HOST primary user's rendered home-manager
# profile is not viable either: that profile is built against the HOST
# closure (fish plugins, nvim treesitter parsers, copilot's `nodejs_latest`)
# and contains `/nix/store` symlinks that the config-seed dereferences into
# plain copies — but the *binaries* they point at are NOT in the guest's
# EROFS store, so the copies would be inert files the guest cannot run.
#
# The shape below keeps every invariant of the sandbox intact:
#   * the guest store stays a build-time EROFS image — no host store share,
#     no host nix daemon (asserted by ../runtime-validation.sh "host
#     /nix/store is not shared");
#   * the guest home stays DISPOSABLE and provisioned at launch time — this
#     module only adds the BINARY set to the closure and a root-owned
#     oneshot that copies a GUEST-BUILT config tree into the home, ordered
#     after the existing `agent-config-seed` so it cannot be overwritten and
#     does not widen the host-staged allowlist;
#   * nothing host-coupled reaches the guest: the vendored neovim.lua is a
#     plain copy under this module directory (no `/nix/store` link), and the
#     fish config is rendered from GUEST pkgs (fish plugins from the
#     guest's own nixpkgs), not copied from the host home.
#
# Cost (deliberate, opt-in, default OFF)
# -------------------------------------
# fish + neovim with treesitter parsers and the plugin/runtime deps the host
# config assumes (grc, the fish plugin set, lazygit) is a sizeable addition to
# the per-class guest closure. The closure is PREBUILT per resource class, so
# enabling this on a host bakes it into every slot of every class. It is
# therefore OFF by default, exactly like the microvm tier itself: a host opts
# in with `myconfig.ai.microvm.guestShellConvenience.enable = true`. Hosts
# that do not enable it are byte-for-byte unchanged (guest.nix guards every
# addition on `agentShellConvenience.enable`, which is false then).
#
# What is NOT carried over from the host config
# ---------------------------------------------
# The host `modules/programs.neovim/default.nix` enables the `copilot-vim`
# plugin, which hardcodes `g:copilot_node_command = "${pkgs.nodejs_latest}/bin/
# node"` — a HOST store path the guest has no equivalent of. It also runs
# `:Copilot setup`, which needs the host's GitHub OAuth. Both are dropped in
# the guest: a sandbox agent editor does not get GitHub Copilot. (neovim
# itself still works; an operator who wants an AI completion inside the
# guest editor uses the agent CLI that is already baked in.) `neovide` is a
# GUI and is irrelevant to a headless guest, so it is not carried over.
#
# The host fish config (`modules/programs.fish/default.nix`) pulls in
# `any-nix-shell` (a nix-shell wrapper concept the guest does not have) and a
# `myconfig.persistence` directory for `.local/share/fish`. Neither applies to
# a disposable guest, so neither is carried over.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;
  convCfg = cfg.guestShellConvenience;

  # The vendored neovim config (a plain copy of the host's
  # `modules/programs.neovim/neovim.lua`, kept here so the guest build does
  # not depend on a host module path). It is path-clean (no `/nix/store`,
  # no `copilot`, no `node` references).
  neovimLua = ./convenience/neovim.lua;
  # The vendored fish `ex` helper, copied verbatim from the host's
  # `modules/programs.fish/functions/ex.fish`.
  fishEx = ./convenience/ex.fish;
  # The vendored fish `config.fish` (the host's abbreviations, init and
  # helper functions, with host-coupled parts dropped). Kept as a plain
  # file so the guest build does not depend on a host module path and there
  # is no nested-quoting risk.
  fishConfigFish = ./convenience/fish-config.fish;

  # --- A GUEST-BUILT neovim with the host's plugin subset ----------------
  # Only the plugins that do NOT require host store paths or host auth.
  # Built from the guest's own pkgs, so every plugin and parser lands in the
  # guest's EROFS store. `copilot-vim`, `lazygit-nvim` (its `<leader>gg`
  # mapping) and `neovide` are deliberately omitted (see the header).
  #
  # Implementation note: rather than the `wrapNeovim`/`makeNeovimConfig` API
  # (whose exact signature varies across nixpkgs revisions and is hard to
  # pin without an evaluable store), this ships plain `pkgs.neovim` and a
  # GUEST-BUILT `nvim-config` tree (init.lua + a `pack/` dir of plugin
  # symlinks) staged into `~/.config/nvim` by the seeder below. neovim loads
  # `pack/*/start/` automatically, so no `&packpath` glue is needed.
  neovimConfig =
    let
      plugins = with pkgs.vimPlugins; [
        vim-commentary
        nvim-tree-lua
        vim-startify
        terminus
        nvim-treesitter
        tokyonight-nvim
        sonokai
        dracula-vim
        gruvbox
        papercolor-theme
        nvim-web-devicons
        bufferline-nvim
        vim-nix
        kdl-vim
        none-ls-nvim
        vim-markdown
      ];
      # neovim's native package loader expects plugins under
      # <packpath>/pack/<name>/start/<plugin>. Layout them so `nvim` picks
      # them up with zero config.
      packDir = pkgs.runCommand "guest-nvim-pack" { } ''
        set -eu
        d=$out/pack/myconfig/start
        mkdir -p "$d"
        ${lib.concatMapStringsSep "\n" (p: ''
          ln -s ${lib.escapeShellArg p} "$d/${p.pname or p.name}"
        '') plugins}
      '';
    in
    pkgs.runCommand "guest-nvim-config"
      {
        preferLocalBuild = true;
      }
      ''
        set -eu
        root=$out/.config/nvim
        mkdir -p "$root"
        cp ${neovimLua} "$root/init.lua"
        # The pack dir: symlink the whole pack tree in.
        ln -s ${packDir}/pack "$root/pack"
      '';

  # --- A GUEST-BUILT fish config tree ------------------------------------
  # A derivation laying out `.config/fish/{config.fish, conf.d/*,
  # functions/*}` with the host's abbreviations, init, the hydro prompt and
  # the `ex` helper — all rendered from the guest's own pkgs. Copied into
  # the disposable home by `agent-shell-convenience.service` below, so it is
  # a CLOSED copy in the guest closure, never a host store link.
  fishConfig =
    pkgs.runCommand "guest-fish-config"
      {
        preferLocalBuild = true;
      }
      ''
        set -eu
        root=$out/.config/fish
        mkdir -p "$root/conf.d" "$root/functions"

        # --- conf.d: the hydro prompt (vendored from the fish plugin) ----
        cp ${pkgs.fishPlugins.hydro}/share/fish/vendor_conf.d/hydro.fish \
           "$root/conf.d/hydro.fish"

        # --- functions: hydro prompt pieces + the host's helpers ---------
        # Copy WHATEVER the plugin ships rather than a hardcoded file list:
        # hydro's set of vendor functions changes across releases (it used to
        # ship `fish_title.fish`, current versions do not), and a missing name
        # would fail this build — and with it every guest of a host that opted
        # into the convenience shell.
        cp ${pkgs.fishPlugins.hydro}/share/fish/vendor_functions.d/*.fish \
           "$root/functions/"
        cp ${fishEx} "$root/functions/ex.fish"
        cp ${fishConfigFish} "$root/config.fish"
      '';

  # The fish plugin RUNTIME the host config enables (the plugin *behavior*
  # the conf.d/functions above rely on: colored-man-pages, done, grc, sponge,
  # z). These are packages that must be on PATH so fish finds their
  # functions; built from the guest's own pkgs.
  fishPluginPackages = with pkgs.fishPlugins; [
    colored-man-pages
    done
    grc
    sponge
    z
  ];

  # The package set folded into the guest's `environment.systemPackages`.
  # EMPTY when the feature is disabled, so a host that does not opt in never
  # forces the neovim/fish derivations and the guest closure is byte-for-byte
  # unchanged. (Nix laziness does not save us here because guest.nix
  # unconditionally concatenates this list, so it must already be empty.)
  packages =
    if convCfg.enable then
      [
        pkgs.neovim
        pkgs.lazygit # neovim's <leader>gg lazygit-nvim runtime
      ]
      ++ lib.optionals (convCfg.shell == "fish") (
        [
          pkgs.fish
          pkgs.grc # the host fish config's grc colourizer
        ]
        ++ fishPluginPackages
      )
    else
      [ ];

  # --- guest-side seeder (mirrors ../config-seed.nix's `seeder`) -----------
  # Root oneshot that copies the GUEST-BUILT fish config tree
  # (`fishConfig`) into the disposable `/home/agent/.config/fish` AFTER
  # `agent-config-seed.service` has seeded the home from the host-staged
  # allowlist, so a host-side fish config (if the operator ever stages one via
  # `configSeed.extraPaths`) would NOT be clobbered by this module's tree:
  # this oneshot only adds `.config/fish`, it does not own the whole home. The
  # copy dereferences store symlinks (the fishConfig tree is a derivation), so
  # the guest gets plain files, never a link into a store it has — but since
  # the store IS the guest's own here, that is a closed copy of the guest's
  # own closure, which is exactly the invariant we want.
  seeder = pkgs.writeShellApplication {
    name = "agent-shell-convenience-apply";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      set -euo pipefail

      readonly HOME_DIR=/home/agent
      readonly OWNER=agent
      readonly GROUP=users
      readonly FISH_SRC=${lib.escapeShellArg fishConfig}
      readonly NVIM_SRC=${lib.escapeShellArg neovimConfig}

      log() { printf 'agent-shell-convenience: %s\n' "$*" >&2; }

      install -d -m 0700 -o "$OWNER" -g "$GROUP" -- "$HOME_DIR"
      install -d -m 0755 -o "$OWNER" -g "$GROUP" -- "$HOME_DIR/.config"

      # --- neovim config (always staged, regardless of login shell) -------
      # nvim loads init.lua and the pack/ dir automatically from ~/.config/nvim.
      if [[ -d "$NVIM_SRC" ]]; then
        rm -rf -- "$HOME_DIR/.config/nvim"
        cp -R -- "$NVIM_SRC/.config/nvim" "$HOME_DIR/.config/nvim"
        chown -R "$OWNER:$GROUP" -- "$HOME_DIR/.config/nvim"
        chmod -R u+rwX,go= -- "$HOME_DIR/.config/nvim"
        log "seeded $HOME_DIR/.config/nvim from $NVIM_SRC"
      else
        log "no nvim config tree at $NVIM_SRC; skipping"
      fi
      ${lib.optionalString (convCfg.shell == "fish") ''
        # --- fish config (only when fish is the login shell) --------------
        if [[ ! -d "$FISH_SRC" ]]; then
          log "no fish config tree at $FISH_SRC; skipping"
          exit 0
        fi
        rm -rf -- "$HOME_DIR/.config/fish"
        cp -R -- "$FISH_SRC/.config/fish" "$HOME_DIR/.config/fish"
        chown -R "$OWNER:$GROUP" -- "$HOME_DIR/.config/fish"
        chmod -R u+rwX,go= -- "$HOME_DIR/.config/fish"
        log "seeded $HOME_DIR/.config/fish from $FISH_SRC"
      ''}
    '';
    meta = with lib; {
      description = "Copy the guest-built fish + neovim config into the disposable agent home (myconfig.ai.microvm)";
      platforms = platforms.linux;
    };
  };

  # The guest NixOS module fragment: the seeder oneshot, ordered AFTER
  # `agent-config-seed` (so it does not overwrite host-staged config) and
  # BEFORE sshd / the batch job controller / the agent-state linker (so the
  # home is fully provisioned before any agent or operator shell starts).
  # An attrset (not a mkIf at THIS level): the whole `agentShellConvenience`
  # arg is only folded into the guest by guest.nix when `enable` is true, so
  # this fragment never even reaches a disabled guest.
  guestModule = {
    systemd.services.agent-shell-convenience = {
      description = "Seed the guest-built fish + neovim config into the disposable agent home";
      wantedBy = [ "multi-user.target" ];
      after = [ "agent-config-seed.service" ];
      # Mirror agent-config-seed's ordering: before sshd, the batch job
      # controller, the agent-state linker and the boot-time model discovery.
      before = [
        "sshd.service"
        "agent-state-link.service"
        "agent-model-config.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = lib.getExe seeder;
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        # It writes the agent's home.
        ProtectHome = false;
        StandardOutput = "journal+console";
        StandardError = "journal+console";
      };
    };
    # Make neovim the system-wide `$EDITOR`/`$VISUAL`, mirroring the host's
    # `programs.neovim.defaultEditor = true`. NixOS' `defaultEditor` sets
    # `environment.variables.EDITOR`/`VISUAL` to `nvim` (and installs the
    # `vim`/`vi` symlinks when `viAlias`/`vimAlias` are on — we ship plain
    # `pkgs.neovim` instead, so set the variables explicitly rather than rely
    # on `defaultEditor`'s alias side effects). Both paths reach the agent:
    # the login shell via `/etc/set-environment` and the non-login batch
    # worker via guest.nix's `environment=` (which already forwards
    # `environment.variables`).
    environment.variables.EDITOR = "nvim";
    environment.variables.VISUAL = "nvim";
  }
  # When fish is the guest `agent` user's LOGIN shell, NixOS asserts that the
  # matching `programs.fish` module is enabled (`users.users.<name>.shell is
  # set to fish, but programs.fish.enable is not true`). That assertion is
  # load-bearing here rather than cosmetic: only the fish module installs
  # `/etc/fish/config.fish` + `nixos-env-preinit.fish`, which is what makes
  # `environment.variables` (the guest's model-endpoint env, EDITOR, ...)
  # and the nix profile directories visible in a fish login shell — fish does
  # NOT read the bash-flavoured `/etc/set-environment` on its own. So enable
  # the module instead of silencing the check with
  # `ignoreShellProgramCheck`.
  // lib.optionalAttrs (convCfg.shell == "fish") {
    programs.fish.enable = true;
  };
in
{
  options.myconfig.ai.microvm.guestShellConvenience = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Bake a small shell-convenience toolset (fish, neovim) into the guest
        closure and render the host's *config source* for them into the
        disposable guest home — so an operator who SSHes into a sandbox gets
        the same fish prompt/abbreviations and the same neovim keybindings
        they have on the host, WITHOUT running home-manager in the guest or
        staging the host's rendered (host-closure-coupled) dotfiles.

        Default OFF: it adds a non-trivial amount to every prebuilt guest
        closure (fish + neovim + treesitter parsers + the plugin/runtime
        deps the host config assumes). Enable it per host with
        `myconfig.ai.microvm.guestShellConvenience.enable = true`.

        Invariants preserved: the guest store stays a build-time EROFS image
        (no host store share, no host nix daemon), the guest home stays
        disposable and provisioned at launch time, and nothing host-coupled
        reaches the guest (the vendored neovim.lua is a plain copy; fish
        plugins are built from the guest's own pkgs).

        Deliberately NOT carried over from the host config: the `copilot-vim`
        plugin (needs host nodejs + GitHub OAuth), `neovide` (a GUI), and
        `any-nix-shell` (a nix-shell concept the guest does not have).
      '';
    };

    shell = mkOption {
      type = types.enum [
        "bash"
        "fish"
      ];
      default = "fish";
      description = ''
        The login shell of the guest `agent` user when convenience is
        enabled. `fish` (the default) gives the operator the host's fish
        prompt and abbreviations; `bash` keeps the historical guest shell but
        still gets neovim and the staged fish config files (unused by bash).
      '';
    };
  };

  # Expose the guest-shell-convenience module argument UNCONDITIONALLY (set
  # outside `mkIf`, like ../default.nix does for `agentRegistry` etc.). Nix
  # laziness means a host that does not enable the feature never forces any
  # of the package derivations below, so the byte-for-byte-unchanged
  # guarantee holds: guest.nix folds them in only under
  # `agentShellConvenience.enable`, which is false then.
  config._module.args.agentShellConvenience = {
    inherit packages;
    # The guest shell: bash when disabled (the historical guest shell), the
    # chosen shell (fish/bash) when enabled.
    shell =
      if convCfg.enable then
        (if convCfg.shell == "fish" then pkgs.fish else pkgs.bashInteractive)
      else
        pkgs.bashInteractive;
    # The guest NixOS module fragment (the seeder oneshot), or an empty
    # attrset when disabled, so guest.nix's `mkMerge` merges nothing.
    guestModule = if convCfg.enable then guestModule else { };
    enable = convCfg.enable;
  };
}

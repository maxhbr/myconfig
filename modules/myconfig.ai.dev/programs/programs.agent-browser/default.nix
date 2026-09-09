# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# agent-browser (https://github.com/vercel-labs/agent-browser) — headless
# browser automation CLI for AI agents: Chrome/Chromium via CDP with
# accessibility-tree snapshots, no Playwright/Puppeteer dependency. The
# package is in nixpkgs (`pkgs/by-name/ag/agent-browser/package.nix`), so no
# extra flake input is needed: this module just consumes `pkgs.agent-browser`.
#
# Runtime browser discovery: agent-browser probes the PATH with its own
# pinned `which` for `google-chrome`, `chromium-browser`, `chromium`,
# `brave-browser`, ... (nixpkgs substitutes the probe with an absolute
# store path), or takes an explicit executable via
# `AGENT_BROWSER_EXECUTABLE_PATH`. On desktop hosts the home-manager
# chromium is on the PATH and found automatically, so this module pins NO
# executable by default — a literal `${pkgs.chromium}/bin/chromium` default
# would drag the whole chromium closure into every `myconfig.ai` host,
# headless ones included.
#
# agent-browser supports the chrome family only (plus the `lightpanda`
# engine); there is no firefox support, hence there is no browser-name
# option. The engine defaults to `chrome`.
#
# Sandboxes: the CLI itself is added to every sandbox tier via
# `myconfig.ai.dev.sandboxTools.extraPackages` and mysbx's `extraTools`, but NOT
# the browser — that stays an explicit per-host opt-in (closure size,
# security surface). Inside a sandbox an agent can either run
# `agent-browser install` (downloads a pinned Chrome into `~/.agent-browser`,
# needs network and a writable home) or the host opts in:
#
#   myconfig.ai.dev.sandboxTools.extraPackages = [ pkgs.chromium ];
#   myconfig.ai.dev.sandboxTools.extraEnv.AGENT_BROWSER_EXECUTABLE_PATH =
#     "${pkgs.chromium}/bin/chromium";
#   myconfig.ai.dev.mysbx.extraTools = [ pkgs.chromium ];
#
# Upstream ships skill content next to the binary: `$out/skills` holds the
# discovery stub (a directory `agent-browser/` with the `SKILL.md` deployed
# to the agent harnesses via the handcrafted skill registry), and
# `$out/skill-data` holds the runtime skill content served by
# `agent-browser skills get <name>` — always matching the installed CLI
# version, resolved by the CLI relative to its own executable, so it needs
# no extra wiring here.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.agent-browser;
in
{
  options.myconfig = with lib; {
    ai.dev.agent-browser = {
      enable = mkEnableOption "myconfig.ai.dev.agent-browser";

      package = mkPackageOption pkgs "agent-browser" { };

      engine = mkOption {
        type = types.enum [
          "chrome"
          "lightpanda"
        ];
        default = "chrome";
        description = ''
          Browser engine agent-browser launches, exported as
          `AGENT_BROWSER_ENGINE`. `chrome` covers Chrome, Chromium, Brave and
          other CDP-compatible browsers; `lightpanda` is the lightweight
          engine for basic automation.
        '';
      };

      browserExecutablePath = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = literalExpression ''"\${pkgs.chromium}/bin/chromium"'';
        description = ''
          Absolute path to the chrome-family executable agent-browser should
          launch, exported as `AGENT_BROWSER_EXECUTABLE_PATH`. `null`
          (default) lets agent-browser discover a browser on the PATH
          (`google-chrome`, `chromium`, `brave-browser`, ...), which finds the
          home-manager chromium on desktop hosts. Set this only to pin a
          browser that is not on the PATH — and remember the package then
          becomes part of the host closure.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # Register the agent-browser skill source (NixOS-level);
    # `skills/default.nix` applies it to every enabled agent harness via
    # the `handcrafted` registry. The entry must point at the directory
    # that directly contains the SKILL.md (see playwright-cli.nix), which
    # is `$out/skills/agent-browser` — NOT `$out/skills`, whose entries
    # are nested one level deeper.
    myconfig.ai.dev.skills.handcrafted.agent-browser = "${cfg.package}/skills/agent-browser";

    home-manager.sharedModules = [
      {
        home.packages = [ cfg.package ];

        home.sessionVariables = {
          AGENT_BROWSER_ENGINE = cfg.engine;
        }
        // lib.optionalAttrs (cfg.browserExecutablePath != null) {
          AGENT_BROWSER_EXECUTABLE_PATH = cfg.browserExecutablePath;
        };
      }
    ];

    # Sandbox tiers: agent-browser needs to exist where agents run — inside
    # the sandboxes, not only on the host.
    #
    # `myconfig.ai.dev.sandboxTools.extraPackages` reaches every tier that
    # consumes the shared list (the `agent-bubblewrap-*`/nono jails, the
    # `myconfig.ai.dev.microvm` guests, the `sandboxed-*` qemu runners and the
    # gVisor image); mysbx has its own `extraTools` extension point (see
    # ../programs.hunk and ../programs.rtk for the same pair of hooks).
    #
    # NOTE: agent-browser requires a browser at runtime. Hosts that want
    # browser automation in sandboxes must also add chromium to the sandbox
    # closures — see the opt-in snippet in the header comment above.
    myconfig.ai.dev.sandboxTools.extraPackages = [ cfg.package ];

    myconfig.ai.dev.mysbx = lib.mkIf config.myconfig.ai.dev.mysbx.enable {
      extraTools = [ cfg.package ];
    };
  };
}

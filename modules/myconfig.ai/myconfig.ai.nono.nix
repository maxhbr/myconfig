# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared options for the `nono` sandbox wrappers (`agent-nono-pi`,
# `agent-nono-opencode`, `agent-nono-claude`, ...). See `./fns/nono-app.nix` for the
# wrapper library and the matching `extraAllowDirs` / `extraReadOnlyDirs` arguments.
{ config, lib, ... }:
{
  options.myconfig.ai.nono = with lib; {
    fwdEnvs = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Environment variables forwarded from the host into every
        `nono-app` wrapper (`agent-nono-pi`, `agent-nono-opencode`,
        `agent-nono-claude`, ...), in addition to `OPENAI_API_KEY` which is
        always forwarded.

        Each name is forwarded only when set on the host, so a sandbox never
        fails to start because an optional variable is unset. This is in
        addition to the per-wrapper defaults (`TERM`, `COLORTERM`, `LANG`,
        `LC_ALL`, `EDITOR`, `VISUAL`) baked into `nono-app.nix`.

        See `modules/myconfig.ai/fns/nono-app.nix` (`extraFwdEnv`) for the
        implementation.
      '';
    };
  };
}

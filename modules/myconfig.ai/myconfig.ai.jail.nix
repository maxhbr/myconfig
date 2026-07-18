# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared options for the `jail-app` wrappers (`jailed-pi`,
# `jailed-opencode`, `jailed-claude`, ...). See `./fns/jail-app.nix` for the
# wrapper library and the matching `extraFwdEnv` / `extraRuntimeEnv` arguments.
{ config, lib, ... }:
{
  options.myconfig.ai.jail = with lib; {
    fwdEnvs = mkOption {
      type = types.listOf types.str;
      default = [ "OPENAI_API_KEY" ];
      description = ''
        Environment variables forwarded from the host into every
        `jail-app` wrapper (`jailed-pi`, `jailed-opencode`,
        `jailed-claude`, ...).

        Each name is forwarded via `try-fwd-env`, i.e. it is passed through
        only when set on the host, so a jail never fails to start because
        an optional variable is unset. This is in addition to the
        per-wrapper defaults (`TERM`, `COLORTERM`, `LANG`, `LC_ALL`,
        `EDITOR`, `VISUAL`) baked into `jail-app.nix`.

        See `modules/myconfig.ai/fns/jail-app.nix` (`extraFwdEnv`) and the
        vendored `try-fwd-env` combinator.
      '';
    };
  };
}

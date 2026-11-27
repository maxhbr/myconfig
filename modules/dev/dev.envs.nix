# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Opt-in per-topic dev sessions instead of global installs: feature modules
# register their toolset under `myconfig.dev.envs.<name>` and this module
# emits one `<name>-dev-env` command into the user's home-manager
# environment. Running it puts the toolset on PATH and `exec`s the user's
# regular login shell (config.users.users.<user>.shell, e.g. fish) —
# nothing from the toolset is installed globally.
#
# Why not home.packages: a global install puts every topic toolchain on
# every shell's PATH, pulls heavy closures into the always-present
# profile and makes home-manager's buildEnv collide on clashing binaries
# (e.g. GNU binutils vs. dev.rust's LLVM bintools). A session command
# keeps the base profile slim, sidesteps collisions entirely (the env
# prepends its own store paths, no buildEnv involved) and makes the
# environment explicit: `embedded-dev-env` enters the firmware session.
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.dev.envs;
  user = myconfig.user;
  userShell = config.users.users."${user}".shell;

  mkDevEnv =
    name: env:
    pkgs.writeShellApplication {
      name = "${name}-dev-env";
      runtimeInputs = env.packages;
      runtimeEnv = {
        # Marks the active session; shells/prompt hooks can display it,
        # and nested invocations simply overwrite it.
        MYCONFIG_DEV_ENV = name;
      };
      text = ''
        exec ${userShell} "$@"
      '';
    };
in
{
  options.myconfig.dev.envs =
    with lib;
    mkOption {
      type = types.attrsOf (
        types.submodule {
          options.packages = mkOption {
            type = types.listOf types.package;
            default = [ ];
            description = ''
              Packages prepended to PATH inside the `<name>-dev-env`
              session. They are not installed globally.
            '';
          };
        }
      );
      default = { };
      description = ''
        Per-topic development environments. Each entry becomes one
        `<name>-dev-env` command that starts the user's regular shell
        with the registered packages on PATH, instead of installing
        those packages globally.
      '';
    };

  config = lib.mkIf (cfg != { }) {
    home-manager.sharedModules = [
      {
        home.packages = lib.mapAttrsToList mkDevEnv cfg;
      }
    ];
  };
}

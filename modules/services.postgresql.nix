# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  lib,
  config,
  ...
}:
{
  # config = (lib.mkIf config.services.postgresql.enable {
  #   services.postgresql = {
  #     # package = pkgs.postgresql_10;
  #     enableTCPIP = true;
  #     authentication = pkgs.lib.mkOverride 10 ''
  #       local all all trust
  #       host all all ::1/128 trust
  #     '';
  #   };
  # });
}

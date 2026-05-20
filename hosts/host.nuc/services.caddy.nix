# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:

{
  # The base hostname `<hostname>.wg0.maxhbr.local` is served by the
  # myconfig.deployedServices module as an index page that links to
  # all configured sub-domains.

  services.caddy.enable = true;
}

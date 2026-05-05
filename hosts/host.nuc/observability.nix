# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:
{
  myconfig.observability = {
    host.enable = true;
    host.uptime.enable = true;
    client.enable = true;
  };
}

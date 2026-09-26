# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  ...
}:

{
  config = {
    myconfig.gitolite = {
      enable = true;
      restrictToWg0 = true;
    };
  };
}

# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./docker.ninfer.cuda.nix
  ];
}

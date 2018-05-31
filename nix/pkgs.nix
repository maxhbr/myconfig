# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
import <nixpkgs> {
  config = import ./nixpkgs-config.nix;
  overlays = import ./nixpkgs-overlays.nix;
}

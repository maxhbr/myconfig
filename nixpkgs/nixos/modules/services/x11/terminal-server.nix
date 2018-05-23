# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
let
  # stable_json = builtins.fromJSON (builtins.readFile ./stable.json);
  # nixpkgs = builtins.fetchTarball {
  #   url = "https://github.com/NixOS/nixpkgs/archive/${stable_json.rev}.tar.gz";
  #   # sha256 = stable_json.outputSha256;
  # };
  nixpkgs = builtins.fetchTarball http://nixos.org/channels/nixos-18.03/nixexprs.tar.xz;
in
  import (nixpkgs + "/nixos/modules/services/x11/terminal-server.nix")

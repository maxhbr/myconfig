#!/usr/bin/env nix-build
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
let
  hostName = "mobile";

  build = import <nixpkgs/nixos> {
    system = "x86_64-linux";
    configuration = import ../machines {
      config = {};
      hostId = "12ABCDEF";
      hostName = hostName;
    };
  };
in build.vm
# in { system = build.config.system.build.toplevel; }

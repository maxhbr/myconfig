# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  config = {
    # generates:
    # - /run/booted-system/pkgs/openjdk8
    # - /run/current-system/pkgs/openjdk8
    # - /nix/var/nix/profiles/system/pkgs/openjdk8
    # - ...
    # see: https://discourse.nixos.org/t/always-symlinking-the-latest-jdk-to-a-certain-path/3099/6
    system.extraSystemBuilderCmds = ''
      mkdir -p $out/pkgs/
      ln -s ${pkgs.openjdk8} $out/pkgs/openjdk8
      ln -s ${pkgs.openjdk11} $out/pkgs/openjdk11
      ln -s ${pkgs.openjdk12} $out/pkgs/openjdk12
    '';

    nixpkgs.overlays = [(self: super: {
      maven = super.maven.override {
        jdk = self.openjdk11;
      };
    })];

    environment = {
      systemPackages = with pkgs; [
        openjdk openjdk8 openjdk11 openjdk12
        maven gradle
      ];
      variables = {
        JAVA_HOME = "/run/current-system/pkgs/openjdk11";
      };
    };
  };
}

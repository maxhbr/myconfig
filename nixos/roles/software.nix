# Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.work = {
      enable = lib.mkEnableOption "Work role";
    };
    myconfig.roles.dev = {
      enable = lib.mkEnableOption "Dev role";
    };
    myconfig.roles.imagework = {
      enable = lib.mkEnableOption "Imagework role";
    };
    myconfig.roles.wine = {
      enable = lib.mkEnableOption "Wine role";
    };
    myconfig.roles.tex = {
      enable = lib.mkEnableOption "Tex role";
    };
    myconfig.roles.sundtek = {
      enable = lib.mkEnableOption "Sundtek role";
    };
  };

  imports = [
################################################################################
    { # work
      config = lib.mkIf config.myconfig.roles.work.enable {
        environment.systemPackages = with pkgs; [
          openvpn networkmanager_openvpn
          # rdesktop
          unstable.openjdk unstable.maven unstable.gradle
          libreoffice
          zoom-us unstable.rambox # franz hipchat
          p7zip
          thrift93
          idea-ultimate
          dia
        ];
      };
    }
################################################################################
    { # dev
      config = lib.mkIf config.myconfig.roles.dev.enable {
        environment.systemPackages = with pkgs; [
          meld
          gnumake cmake automake
          cloc
          gitAndTools.gitFull
          unstable.gitAndTools.tig

          python python3

          unstable.stack unstable.cabal-install unstable.cabal2nix
        ] ++ (with pkgs.unstable.haskellPackages; [
          # cabal-install
          ghc hlint pandoc
          pointfree pointful
        ]) ++ (with pkgs.haskellPackages; [
            hdevtools
        ]);
      };
    }
################################################################################
    { # imagework
      config = lib.mkIf config.myconfig.roles.imagework.enable {
        environment.systemPackages = with pkgs; [
          gimp-with-plugins
          rawtherapee
          unstable.geeqie
          # krita
          # inkscape

          # blender
          librecad
        ];
      };
    }
################################################################################
    { # wine
      config = lib.mkIf config.myconfig.roles.wine.enable {
        environment.systemPackages = with pkgs; [
          wineStaging
          winetricks
        ];
      };
    }
################################################################################
    { # tex
      config = lib.mkIf config.myconfig.roles.tex.enable {
        environment.systemPackages = with pkgs; [
          (pkgs.texLiveAggregationFun {
            paths = [
              pkgs.texLive pkgs.texLiveExtra
              pkgs.texLiveBeamer
              pkgs.texLiveCMSuper
            ];
          })
        ];
      };
    }
################################################################################
    { # sundtek
      config = lib.mkIf config.myconfig.roles.sundtek.enable {
        environment.systemPackages = with pkgs; [
          sundtek
        ];
      };
    }
  ];
}

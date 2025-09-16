# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  is_full_build = false;
in
{
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ./ai.brain.nix
    ../host.workstation/gaming/games.steam
    (myconfig.metadatalib.setupAsBuildMachine [
      myconfig.metadatalib.get.hosts.p14.pubkeys."id_ed25519_no_pw.pub"
    ])
    { environment.systemPackages = with pkgs; [ linuxPackages.usbip ]; }
    {
      # programs.mosh.enable = lib.mkDefault true;
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
  ];

  config = {
    networking.hostName = "brain";
    networking.hostId = "71824632";
    networking.useDHCP = false;
    # networking.interfaces.wlp0s20f3.useDHCP = true;
    # networking.interfaces.enp82s0u2u1u2.useDHCP = true;
    myconfig = {
      desktop = {
        enable = is_full_build;
        wayland = {
          enable = true;
          selectedSessions = [
            "niri"
            "niri-plain"
            # "labwc"
            # "river"
            "plasma6"
          ];
          niri.additionalConfigKdl = ''
            // You can configure outputs by their name, which you can find
            // by running `niri msg outputs` while inside a niri instance.
            // The built-in laptop monitor is usually called "eDP-1".
            // Find more information on the wiki:
            // https://github.com/YaLTeR/niri/wiki/Configuration:-Outputs
            // Remember to uncomment the node by removing "/-"!
            output "eDP-1" {
                // Uncomment this line to disable this output.
                // off

                // Resolution and, optionally, refresh rate of the output.
                // The format is "<width>x<height>" or "<width>x<height>@<refresh rate>".
                // If the refresh rate is omitted, niri will pick the highest refresh rate
                // for the resolution.
                // If the mode is omitted altogether or is invalid, niri will pick one automatically.
                // Run `niri msg outputs` while inside a niri instance to list all outputs and their modes.
                mode "1920x1080@60.000"

                // Scale is a floating-point number, but at the moment only integer values work.
                scale 1.0

                // Transform allows to rotate the output counter-clockwise, valid values are:
                // normal, 90, 180, 270, flipped, flipped-90, flipped-180 and flipped-270.
                transform "normal"

                // Position of the output in the global coordinate space.
                // This affects directional monitor actions like "focus-monitor-left", and cursor movement.
                // The cursor can only move between directly adjacent outputs.
                // Output scale and rotation has to be taken into account for positioning:
                // outputs are sized in logical, or scaled, pixels.
                // For example, a 3840×2160 output with scale 2.0 will have a logical size of 1920×1080,
                // so to put another output directly adjacent to it on the right, set its x to 1920.
                // If the position is unset or results in an overlap, the output is instead placed
                // automatically.
                position x=0 y=0
            }
          '';
        };
      };
      virtualisation.enable = is_full_build;
      # dev = {
      #   compliance.enable = true;
      # };
    };
    virtualisation = {
      docker.enable = is_full_build;
      podman.enable = true;
      oci-containers = {
        backend = "podman";
      };
      # virtualbox.host.enable = true;
      # lxc.enable = true;
      libvirtd.enable = is_full_build;
    };
    services.hardware.bolt.enable = false;

    services.gnome.gnome-keyring.enable = true;

    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };

      kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    };

    hardware.enableRedistributableFirmware = true;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}

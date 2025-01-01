{ lib, pkgs, inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    {
      environment.systemPackages = with pkgs; [ x11vnc ];
      ## Setup via ssh tunnel:
      # $ ssh -t -L 5900:localhost:5900 $IP 'x11vnc -ncache 10 -unixpw -localhost -display :0'
      ## in other terminal:
      # $ vncviewer -encodings 'copyrect tight zrle hextile' localhost:0
      ## or open ports
      # networking.firewall.allowedUDPPorts = [ 5900 ];
      # networking.firewall.allowedTCPPorts = [ 5900 ];
    }
  ];

  config = {
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "22.11"; # Did you read the comment?

    myconfig = {
      desktop.enable = true;
      headless.enable = true;
    };
    virtualisation.docker.enable = true;
    virtualisation.podman.enable = true;

    networking.hostName = "pi4";
    networking.hostId = "ac8edd7a";

    services.vsftpd.enable = lib.mkForce false; # fails to build on arm

    swapDevices = [{
      device = "/swapfile";
      priority = 0;
      size = 4096;
    }];

    # https://github.com/NixOS/nixpkgs/issues/154163
    # https://github.com/NixOS/nixpkgs/issues/111683#issuecomment-968435872
    # https://github.com/NixOS/nixpkgs/issues/126755#issuecomment-869149243
    nixpkgs.overlays = [
      (final: super: {
        makeModulesClosure = x:
          super.makeModulesClosure (x // { allowMissing = true; });
      })
    ];
  };
}

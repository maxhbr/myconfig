{ config, pkgs, ... }:

{
  imports = [
    ../profiles/desktop.nix
    ../profiles/virtualization.nix
    ../profiles/mail.nix
    # ../profiles/dev.nix
    # ../profiles/work.nix
    ../profiles/imagework.nix
    ../profiles/games.nix
  ];

  services.xserver.videoDrivers = ["nvidia"];

}

{ config, lib, pkgs, ... }:

with lib;
rec{
  system.activationScripts =
  {
    # Configure various dotfiles.
    dotfiles = stringAfter [ "users" ]
    ''
      cd /home/mhuber
    '';
  };
}

{ config, pkgs, ... }:

{
  system.activationScripts =
  {
    # Configure various dotfiles.
    dotfiles = stringAfter [ "users" ]
    ''
      cd /home/mhuber
      ln -fs ${./dotfiles/ghci/.ghci} .ghci
    '';
  }
}

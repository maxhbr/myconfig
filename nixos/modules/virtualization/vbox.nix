{ pkgs, ... }:
{
  imports = [
    # paths to other modules
  ];

  options = {
    # option declarations
  };

  config = {
    environment.systemPackages = with pkgs; [vagrant];
    virtualisation.virtualbox.host.enable = true;
  };
}

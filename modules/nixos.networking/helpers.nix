{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = {
    environment.interactiveShellInit = ''
      myPorts() { /run/wrappers/bin/sudo ${pkgs.iproute2}/bin/ss -tulpen; }
    '';

    home-manager.sharedModules = [
      {
        programs.fish = {
          functions = {
            myPorts = "/run/wrappers/bin/sudo ${pkgs.iproute2}/bin/ss -tulpen";
          };
        };
      }
    ];
  };
}

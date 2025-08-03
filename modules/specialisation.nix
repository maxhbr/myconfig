{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    (
      {
        lib,
        config,
        pkgs,
        ...
      }:
      {
        config = lib.mkIf (config.specialisation != { }) {
          # Config that should only apply to the default system, not the specialised ones
        };
      }
    )
  ];
  config = {
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "specialise-unload" "sudo /nix/var/nix/profiles/system/bin/switch-to-configuration test")
    ];
    specialisation = {
      experimental = {
        inheritParentConfig = true;
        configuration = {
        };
      };
    };
  };
}

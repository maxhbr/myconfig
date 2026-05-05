{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    upg = {
      enable = mkEnableOption "upg";
      otherHosts = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of other hosts to generate upg scripts for";
      };
    };
  };
  config = lib.mkIf cfg.upg.enable {
    home-manager.sharedModules = [
      (
        {
          pkgs,
          config,
          lib,
          ...
        }:
        {
          home.packages =
            with pkgs;
            let
              mk-upg-script =
                name: args:
                pkgs.writeShellScriptBin name ''
                  set -euo pipefail
                  set -x
                  exec ${config.home.homeDirectory}/myconfig/priv/switch.sh ${args} "$@"
                '';
            in
            [
              (mk-upg-script "upg" "")
              (mk-upg-script "upg-fast" "--fast")
            ]
            ++ (map (hn: (mk-upg-script "upg-${hn}" "--fast ${hn}")) cfg.upg.otherHosts);
        }
      )
      (
        { config, ... }:
        {
          programs.mr = {
            enable = lib.mkDefault true;
            settings = {
              "myconfig/myconfig/" = {
                checkout = "git clone https://github.com/maxhbr/myconfig";
                update = "git pull --rebase";
              };
              "myconfig/myphoto/" = {
                checkout = "git clone https://github.com/maxhbr/myphoto";
                update = "git pull --rebase";
              };
              "myconfig/nixos/nixpkgs/" = {
                checkout = "git clone https://github.com/NixOS/nixpkgs/";
                update = "git pull --rebase";
              };
              "myconfig/nixos/nixos-hardware/" = {
                checkout = "git clone https://github.com/NixOS/nixos-hardware";
                update = "git pull --rebase";
              };
              "myconfig/nixos/home-manager/" = {
                checkout = "git clone https://github.com/nix-community/home-manager";
                update = "git pull --rebase";
              };
            };
          };
        }
      )
    ];
  };
}

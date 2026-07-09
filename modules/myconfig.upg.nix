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
          super,
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
              # Use the system nix (config.nix.package) instead of the
              # nix version sbomnix pulls in by default.
              # NOTE: `sbomnix.override { nix = ...; }` does *not* propagate
              # into the wrapper PATH -- the callPackage `nix` argument is
              # ignored by the final `makeWrapperArgs` (a buildPythonApplication
              # finalAttrs quirk). Rebuilding `makeWrapperArgs` via
              # `overridePythonAttrs` actually swaps the nix on PATH.
              sbomnix-with-system-nix = sbomnix.overridePythonAttrs (_old: {
                makeWrapperArgs = [
                  "--prefix PATH : ${
                    pkgs.lib.makeBinPath [
                      git
                      super.nix.package
                      python3.pkgs.graphviz
                      nix-visualize
                      vulnix
                      grype
                    ]
                  }"
                ];
              });
            in
            [
              (mk-upg-script "upg" "")
              (mk-upg-script "upg-fast" "--fast")
              sbomnix-with-system-nix
              nvd
            ]
            ++ (map (hn: (mk-upg-script "upg-${hn}" "--fast ${hn}")) cfg.upg.otherHosts);
        }
      )
      (
        { config, pkgs, ... }:
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

          systemd.user.services.mr-update-nixos = {
            Unit = {
              Description = "Run mr update in ~/myconfig/nixos/";
            };
            Service = {
              Type = "oneshot";
              WorkingDirectory = "${config.home.homeDirectory}/myconfig/nixos";
              ExecStart = "${pkgs.mr}/bin/mr update";
            };
          };

          systemd.user.timers.mr-update-nixos = {
            Unit = {
              Description = "Daily mr update in ~/myconfig/nixos/ at 04:00";
            };
            Timer = {
              OnCalendar = "*-*-* 04:00:00";
              RandomizedDelaySec = "30m";
              Persistent = true;
            };
            Install = {
              WantedBy = [ "timers.target" ];
            };
          };
        }
      )
    ];
  };
}

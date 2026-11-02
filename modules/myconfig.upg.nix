{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:

let
  cfg = config.myconfig;
  hostName = config.networking.hostName;
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
              switchSh = "${config.home.homeDirectory}/myconfig/priv/switch.sh";
              mk-upg-script =
                name: args:
                pkgs.writeShellScriptBin name ''
                  set -euo pipefail
                  set -x
                  exec ${switchSh} ${args} "$@"
                '';
              # The hosts whose `managed_by` in hosts/metadata.json points at
              # this one, i.e. the boxes this host deploys for.
              managedHosts = lib.remove hostName (myconfig.metadatalib.getManagedHosts hostName);
              # Deploy every managed host after this one. A failing host is
              # collected instead of aborting, so one unreachable box does not
              # keep the rest from being updated.
              managedHostsStep = lib.optionalString (managedHosts != [ ]) ''
                failed=()
                for host in ${lib.escapeShellArgs managedHosts}; do
                  ${switchSh} --fast "$host" || failed+=("$host")
                done
                if [[ ''${#failed[@]} -gt 0 ]]; then
                  echo "failed to update: ''${failed[*]}" >&2
                  exit 1
                fi
              '';
              # Update this host exactly like `upg`, then the hosts it manages
              # in `--fast` mode: the recursive flake update of the first step
              # already produced their lock file, and `switch.sh` skips that
              # update on every host but the lead host anyway.
              upg-all = pkgs.writeShellScriptBin "upg-all" ''
                set -euo pipefail
                set -x
                ${switchSh}
                ${managedHostsStep}
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
              upg-all
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

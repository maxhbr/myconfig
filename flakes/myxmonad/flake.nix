{
  description = "my xmonad configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      allSystems = [ "x86_64-linux" "aarch64-linux" ];
      eachDefaultSystem = inputs.flake-utils.lib.eachSystem allSystems;
    in eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        my-mute-telco = pkgs.callPackage ./myMuteTelco { inherit pkgs; };
        my-xmobar = pkgs.callPackage ./myXmobar { inherit pkgs my-mute-telco; };
        my-xmonad = pkgs.haskellPackages.callPackage ./myXMonad {
          inherit pkgs my-xmobar my-mute-telco;
        };
        myxev = pkgs.writeShellScriptBin "myxev" ''
          ${pkgs.xorg.xev}/bin/xev -id $(${pkgs.xdotool}/bin/xdotool getactivewindow)
        '';
      in {
        packages = { inherit my-mute-telco my-xmobar my-xmonad; };

        nixosModule = { config, lib, pkgs, ... }: {
          config = (lib.mkIf config.services.xserver.enable {
            environment.variables = {
              XSECURELOCK_BLANK_TIMEOUT = "-1";
              XSECURELOCK_COMPOSITE_OBSCURER = "0";
              # XSECURELOCK_NO_COMPOSITE = "1";
            };

            # system.activationScripts.cleanupXmonadState = "rm $HOME/.xmonad/xmonad.state || true";

            services = {
              xserver = {
                displayManager.defaultSession = "none+myXmonad";
                windowManager = {
                  session = lib.singleton {
                    name = "myXmonad";
                    start = ''
                      exec &> >(tee -a /tmp/myXmonad.log)
                      echo -e "\n\n$(date)\n\n"
                      ${my-xmonad}/bin/xmonad &
                      waitPID=$!
                    '';
                  };
                };

                desktopManager.xterm.enable = false;
              };
            };
          });
        };
        hmModule = { config, lib, pkgs, ... }: {
          imports = [ ./picom.hm.nix ];
          home.packages = with pkgs; [
            my-xmonad
            my-xmobar
            my-mute-telco

            dzen2
            myxev
          ];
          xsession.windowManager.command = "${my-xmonad}/bin/xmonad";
          home.file = {
            ".myXmonadBinary" = {
              text = ''
                "${my-xmonad}/bin/xmonad"
              '';
              onChange = ''
                if [[ -v DISPLAY ]] ; then
                  echo "Restarting xmonad"
                  $DRY_RUN_CMD ${config.xsession.windowManager.command} --restart
                fi
              '';
            };
          };
        };
      });
}

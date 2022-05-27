{
  description = "my xmonad configuration";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; };

  outputs = { self, ... }@inputs: {
    nixosModule = { config, lib, pkgs, ... }:
      let
        my-mute-telco = pkgs.callPackage ./myMuteTelco { inherit pkgs; };
        my-xmobar = pkgs.callPackage ./myXmobar { inherit pkgs my-mute-telco; };
        my-xmonad = pkgs.haskellPackages.callPackage ./myXMonad {
          inherit pkgs my-xmobar my-mute-telco;
        };
        myxev = pkgs.writeShellScriptBin "myxev" ''
          ${pkgs.xorg.xev}/bin/xev -id $(${pkgs.xdotool}/bin/xdotool getactivewindow)
        '';
      in {
        imports = [
          (lib.mkIf config.services.xserver.desktopManager.xfce.enable {
            services.xserver.desktopManager = {
              xterm.enable = false;
              xfce = {
                noDesktop = true;
                enableXfwm = false;
                enableScreensaver = false;
              };
            };
          })
        ];
        config = (lib.mkIf config.services.xserver.enable {
          environment.variables = {
            XSECURELOCK_BLANK_TIMEOUT = "-1";
            XSECURELOCK_COMPOSITE_OBSCURER = "0";
            # XSECURELOCK_NO_COMPOSITE = "1";
          };

          # system.activationScripts.cleanupXmonadState = "rm $HOME/.xmonad/xmonad.state || true";

          services = {
            xserver = {
              serverFlagsSection = ''
                Option "MaxClients" "2048"
              '';
              displayManager.defaultSession =
                if config.services.xserver.desktopManager.xfce.enable then
                  "xfce+myXmonad"
                else
                  "none+myXmonad";
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

          home-manager.sharedModules = [{
            # imports = [ ./picom.hm.nix ];
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
                    ${my-xmonad}/bin/xmonad &
                  fi
                '';
              };
            };
          }];
        });
      };
  };
}

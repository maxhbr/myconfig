{ config, lib, pkgs, ... }:
let
  doomPrivateDir = let
    doomPrivateDeriv = pkgs.stdenv.mkDerivation rec {
      name = "doomPrivateDeriv-1.0";
      src = ./doom.d;
      installPhase = ''
        mkdir -p "$out"
        cp -r * "$out"
      '';
    };
  in "${doomPrivateDeriv}";
  doom-emacs-conf = pkgs: {
    inherit doomPrivateDir;
    extraPackages = with pkgs; [
      mu
      gnuplot
      (nerdfonts.override { fonts = [ "Inconsolata" ]; })
      emacs-all-the-icons-fonts
      shellcheck
    ];
    extraConfig = ''
      (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
    '';
    # dependencyOverrides = nix-doom-emacs.inputs;
  };
in {
  config = {
    environment = {
      variables = { EDITOR = "emacs -nw"; };
      # shellAliases = { vim = "emacs -nw"; };
    };
    home-manager.sharedModules = [
      ({ config, ... }:
        let
          xclipedit = pkgs.writeShellScriptBin "xclipedit" ''
            set -euo pipefail
            tempfile="$(mktemp)"
            trap "rm $tempfile" EXIT
            if [ -t 0 ]; then
                xclip -out > "$tempfile"
            else
                cat > "$tempfile"
            fi
            $EDITOR "$tempfile"
            ${pkgs.xclip}/bin/xclip < "$tempfile"
          '';
        in {
          programs.doom-emacs = (doom-emacs-conf pkgs) // { enable = true; };
          home.packages = with pkgs; [ xclipedit ];
          programs.zsh.shellAliases = {
            magit = ''emacs -e "(magit-status \"$(pwd)\")"'';
          };
          home.file = {
            ".doom.d/imports" = {
              source = ./doom.d-imports;
              recursive = true;
            };
          };
        })
    ];
    services.xserver.windowManager.session = let
      loadScript = pkgs.writeText "emacs-exwm-load" ''
        (require 'exwm)
        (exwm-enable)
        (require 'exwm-config)
        (exwm-config-default)
      '';
    in lib.singleton {
      name = "exwm";
      start = ''
        emacs -l ${loadScript}
      '';
    };
  };
}

{ inputs, pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
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
in {
  imports = [
    ({ ... }: {
      nixpkgs.overlays = [ inputs.emacs.overlay ];
      home-manager.sharedModules = [ inputs.nix-doom-emacs.hmModule ];
    })
  ];
  options.myconfig = with lib; {
    editor.emacs.enable = mkEnableOption "emacs";
  };
  config = lib.mkIf config.myconfig.editor.emacs.enable {
    environment = { variables = { EDITOR = "emacs -nw"; }; };
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
          programs.doom-emacs = {
            enable = true;
            inherit doomPrivateDir;
            extraPackages = with pkgs; [
              gnuplot
              (nerdfonts.override { fonts = [ "Inconsolata" ]; })
              emacs-all-the-icons-fonts
              shellcheck
            ];
          };
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
  };
}

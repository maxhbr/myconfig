{ pkgs, ... }:
let
  my-install-doom = with pkgs;
    writeShellScriptBin "my-install-doom" ''
      set -ex
      DOOMDIR="$HOME/.doom-emacs.d"
      if [[ ! -d "$DOOMDIR" ]]; then
      	git clone https://github.com/hlissner/doom-emacs.git $DOOMDIR
        $DOOMDIR/bin/doom -y install
      fi
      $DOOMDIR/bin/doom upgrade
      $DOOMDIR/bin/doom sync
    '';

in {
  imports = [
    (let
      doom-emacs = pkgs.unstable.callPackage (builtins.fetchTarball {
        url = "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
      }) {
        doomPrivateDir = ./doom.d; # Directory containing your config.el init.el and packages.el files
      };
    in {
      #home-manager.users.mhuber = { home.packages = [ doom-emacs ]; }; 
      config = {
        environment = {
          shellAliases = {
            doom-emacs-alt =
              "${doom-emacs}/bin/emacs --debug-init --with-profile doom";
          };
        };
      };
    })
  ];
  config = {
    environment = {
      systemPackages = with pkgs; [
        my-install-doom
        shellcheck
        coreutils
        fd
        clang
      ];
      shellAliases = {
        doom-emacs = "${pkgs.emacs}/bin/emacs --debug-init --with-profile doom";
      };
    };
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ ];
      home.file = {
        # ".doom-emacs.d" = {
        #   source = builtins.fetchTarball {
        #     url =
        #       "https://github.com/hlissner/doom-emacs/archive/develop.tar.gz";
        #   };
        #   recursive = true;
        # };
        ".doom.d" = {
          source = ./doom.d;
          recursive = true;
          onChange = "${my-install-doom}/bin/my-install-doom";
        };
      };
    };
  };
}

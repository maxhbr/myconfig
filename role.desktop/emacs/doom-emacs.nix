{ pkgs, ... }:
let
  doom-emacs = pkgs.unstable.callPackage (builtins.fetchTarball {
    url = "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
  }) {
    doomPrivateDir =
      ./doom.d; # Directory containing your config.el init.el and packages.el files
  };
  doom-emacs-bin = pkgs.writeShellScriptBin "doom-emacs" ''
    exec "${doom-emacs}/bin/emacs" --with-profile empty "$@"
  '';
  doom-emacsclient-bin = pkgs.writeShellScriptBin "doom-emacsclient" ''
    exec "${doom-emacs}/bin/emacsclient" --with-profile empty "$@"
  '';
in {
  config = {
    environment = {
      systemPackages = with pkgs; [
        doom-emacs-bin
        doom-emacsclient-bin
        shellcheck
      ];
    };
  };
}

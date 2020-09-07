{ pkgs, ... }:
let
  jsonFile = ./. + "/nix-doom-emacs.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
  doom-emacs = pkgs.unstable.callPackage (builtins.fetchGit {
    url = "https://github.com/vlaci/nix-doom-emacs.git";
    inherit (json) rev ref;
  }) {
    doomPrivateDir =
      ./doom.d; # Directory containing your config.el init.el and packages.el files
    extraPackages = epkgs: [ pkgs.mu ];
    extraConfig = ''
      (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
    '';
  };
  doom-emacs-bin = pkgs.writeShellScriptBin "doom-emacs" ''
    exec "${doom-emacs}/bin/emacs" --with-profile empty "$@"
  '';
  # doom-emacsclient-bin = pkgs.writeShellScriptBin "doom-emacsclient" ''
  #   exec "${doom-emacs}/bin/emacsclient" "$@"
  # '';
in {
  config = {
    environment = {
      systemPackages = with pkgs; [
        doom-emacs-bin
        shellcheck
      ];
    };
  };
}

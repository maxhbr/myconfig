{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.programs.mu.enable {
    home.packages = with pkgs; [
      (writeShellScriptBin "runMuIndex" ''
        MAILDIR="$HOME/Maildir"
        if [[ -d "$MAILDIR" ]]; then
          cd "$MAILDIR"
          # pkill -2 -u $UID mu
          sleep 1
          ${mu}/bin/mu index
        fi
      '')
      (writeShellScriptBin "mu4e" "exec emacs -e mu4e")
      mu
    ];
    programs.emacs = {
      extraPackages = epkgs: [
        epkgs.mu4e
        epkgs.mu4e-alert
      ];
      extraConfig = ''
        (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
        (load "${./mu4e-base-config.el}")
      '';
    };
  };
}

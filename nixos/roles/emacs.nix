{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.emacs = {
      enable = lib.mkEnableOption "Emacs role";
    };
  };

  config = lib.mkIf config.myconfig.roles.emacs.enable {
    systemd.user.services.emacs = {
      description = "Emacs: the extensible, self-documenting text editor";
      serviceConfig = {
        Type      = "forking";
        ExecStart = "${pkgs.emacs}/bin/emacs --daemon --user=mhuber";
        ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart   = "always";
      };
      wantedBy = [ "default.target" ];
      environment = {
        SSH_AUTH_SOCK = "%t/keyring/ssh";
        # Make sure aspell will find its dictionaries
        ASPELL_CONF   = "dict-dir /run/current-system/sw/lib/aspell";
        # Make sure locate will find its database
        LOCATE_PATH   = "/var/cache/locatedb";
      };
      enable = true;
    };
  };
}

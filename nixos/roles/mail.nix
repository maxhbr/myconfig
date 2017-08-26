{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.mail = {
      enable = lib.mkEnableOption "Mail role";
    };
  };

  config = lib.mkIf config.myconfig.roles.mail.enable {
    environment.systemPackages = with pkgs; [
      neomutt # mutt-with-sidebar
      offlineimap msmtp gnupg abook notmuch urlview
      sxiv
      procmail
    ];

    services.offlineimap.enable = true;
  };
}

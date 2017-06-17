{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    neomutt # mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch urlview
    sxiv
    procmail
 ];

  services.offlineimap = {
    enable = true;
  };
}

{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch
  ];

  # systemd.user.services.offlineimap = {
  #   description = "Start offlineimap as a daemon";
  #   serviceConfig = {
  #     Type       = "forking";
  #     ExecStart  = "${pkgs.offlineimap}/bin/offlineimap";
  #     KillSignal = "SIGUSR2";
  #     Restart    = "always";
  #   };
  #   wantedBy = [ "multi-user.target" ];
  #   wants = [ "network-online.target" ];
  #   after = [ "network.target" ];
  #   enable = true;
  # };
}

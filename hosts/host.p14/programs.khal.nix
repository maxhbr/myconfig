{
  config,
  lib,
  pkgs,
  ...
}:
{
  home-manager.sharedModules = [
    {
      home.packages = with pkgs; [
        khal
        vdirsyncer
      ]; # calcurse

      # systemd.user.timers.vdirsyncer = {
      #   Unit = { Description = "Timer to synchronize calendars"; };

      #   Timer = {
      #     OnBootSec = "15min";
      #     OnUnitActiveSec = "30min";
      #   };

      #   Install.WantedBy = [ "timers.target" ];
      # };

      # systemd.user.services.vdirsyncer = {
      #   Unit = {
      #     Description = "Synchronize your calendars";
      #     After = [ "network-online.target" ];
      #     Wants = [ "network-online.target" ];
      #   };

      #   Install.WantedBy = [ "default.target" ];

      #   Service = {
      #     ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
      #     Restart = "on-failure";
      #     Type = "oneshot";
      #     RestartSec = 30;
      #   };
      # };
    }
  ];
}

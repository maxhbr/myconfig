{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
{
  # copied from https://github.com/dnut/clipboard-sync/blob/master/flake.nix to not include their nixosMoudle
  options = {
    services.clipboard-sync.enable = lib.mkEnableOption "clipboard-sync";
  };

  config = lib.mkIf (config.services.clipboard-sync.enable && pkgs.stdenv.hostPlatform.isx86_64) {
    systemd.user.services.clipboard-sync = {
      description = "Synchronize clipboards across all displays";
      documentation = [ "https://github.com/dnut/clipboard-sync/" ];
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      requisite = [ "graphical-session.target" ];
      serviceConfig.ExecStart = "/usr/bin/env ${
        inputs.clipboard-sync.packages.${pkgs.system}.default
      }/bin/clipboard-sync --hide-timestamp --log-level debug";
      serviceConfig.Restart = "on-failure";
    };
  };
}

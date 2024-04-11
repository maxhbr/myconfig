# see: https://github.com/NixOS/nixpkgs/issues/97379#issuecomment-1228258543
{...}:
{
  home-manager.sharedModules = [
    ({ config, lib, pkgs, ... }:
      with lib;
      let
        cfg = config.programs.activitywatch;
      in
      {
        options.programs.activitywatch = with types; {
          enable = mkEnableOption "Enable activitywatch";
          package = mkOption { type = package; default = pkgs.activitywatch; };
        };

        config = mkMerge [
          (mkIf cfg.enable {
            home.packages = with pkgs; [ cfg.package ];
            systemd.user.services.activitywatch = {
              Unit.Description = "activitywatch tray-icon";
              Service.ExecStart = (pkgs.writeShellScript "activitywatch-start" ''
                export PATH=$PATH:${makeBinPath [cfg.package]}
                aw-qt
              '').outPath;
              Service.Restart = "on-failure";
              Install.WantedBy = [ "graphical-session.target" ];
            };
          })
        ];
      }
    )
  ];
}

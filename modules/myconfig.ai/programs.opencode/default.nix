{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.opencode = {
      enable = mkEnableOption "myconfig.ai.opencode";
    };
  };
  config = lib.mkIf config.myconfig.ai.opencode.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          opencode
          (pkgs.callPackage ./opencode-bwrap.nix { })
        ];
      }
    ];
  };
}
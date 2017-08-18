{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.tex = {
      enable = lib.mkEnableOption "Tex role";
    };
  };

  config = lib.mkIf config.myconfig.roles.tex.enable {
    environment.systemPackages = with pkgs; [
      (pkgs.texLiveAggregationFun {
        paths = [
          pkgs.texLive pkgs.texLiveExtra
          pkgs.texLiveBeamer
          pkgs.texLiveCMSuper
        ];
      })
    ];
  };
}

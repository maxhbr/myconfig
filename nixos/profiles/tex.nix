{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (pkgs.texLiveAggregationFun {
      paths = [
        pkgs.texLive pkgs.texLiveExtra
        pkgs.texLiveBeamer
        pkgs.texLiveCMSuper
      ];
    })
  ];
}

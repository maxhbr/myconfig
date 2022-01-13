{ config, lib, pkgs, ... }: {
  config = {
    environment.systemPackages = with pkgs; [
      (aspellWithDicts (d: [ d.en d.de ]))
      hunspell
    ];
    environment = {
      variables = { DICPATH = "/run/current-system/sw/share/hunspell"; };
    };
  };
}

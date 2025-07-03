{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    environment.systemPackages = with pkgs; [
      (aspellWithDicts (d: [
        d.en
        d.de
      ]))
      hunspell
      hunspellDicts.de-de
      hunspellDicts.en-us-large
      hunspellDicts.en-gb-large
    ];
    environment = {
      pathsToLink = [
        "/share/hunspell"
        "/share/myspell"
        "/share/hyphen"
      ];
      variables = {
        DICPATH = "/run/current-system/sw/share/hunspell:/run/current-system/sw/share/hyphen";
      };
    };
  };
}

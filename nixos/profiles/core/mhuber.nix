{ config, pkgs, lib, ... }:

let
  hasVBox = config.virtualisation.virtualbox.host.enable;
  hasDocker = config.virtualisation.docker.enable;
  hasnm = config.networking.networkmanager.enable;

in {
  users = {
    mutableUsers = true; # one needs to change the password?
    extraUsers.mhuber = {
      isNormalUser = true;
      group = "mhuber";
      uid = 1000;
      extraGroups = [
        "wheel"
        "audio" "video"
        "dialout"
        "input" ]
        ++ pkgs.lib.optional hasnm "networkmanager"
        ++ pkgs.lib.optional hasVBox "vboxusers"
        ++ pkgs.lib.optional hasDocker "docker";
      home = "/home/mhuber";
      createHome = true;
      shell = "/run/current-system/sw/bin/zsh";
      # password = "dummy";
      initialPassword = lib.mkForce "dummy";
    };
    extraGroups.mhuber.gid = 1000;
  };
  systemd.tmpfiles.rules = [ "d /home/mhuber/tmp 1777 mhuber mhuber 10d" ];

  time.timeZone = "Europe/Berlin";

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "neo";
    defaultLocale = "de_DE.UTF-8";
  };
}

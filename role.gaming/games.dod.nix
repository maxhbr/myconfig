{ pkgs, config, ... }:
let user = config.myconfig.user;
    dod = pkgs.callPackage ../pkgs/DungeonsofDredmor { };
in { config = { home-manager.users."${user}" = { home.packages = [ dod ]; }; }; }

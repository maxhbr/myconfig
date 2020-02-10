{ config, pkgs, lib, ... }:
let
  myborgbackup_sh = with pkgs; writeScriptBin "myborgbackup.sh" (lib.fileContents ./myborgbackup.sh);
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        myborgbackup_sh
      ];
    };
  };
}

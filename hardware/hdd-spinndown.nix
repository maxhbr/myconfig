{ pkgs, ... }:
let
  spindownAllHdds =
    with pkgs;
    writeScriptBin "spindownAllHdds" ''
      rotHdds() {
        ${util-linux}/bin/lsblk -dnp -o name,rota |
            ${gnugrep}/bin/grep \'.*\\s1\' |
            ${coreutils}/bin/cut -d \' \' -f 1
      }
      if [[ $# -eq 0 ]] ; then
        ${hdparm}/bin/hdparm -S 240 -B 127 $(rotHdds)
      else
        ${hdparm}/bin/hdparm -S 240 -B 127 $@
      fi
    '';
in
{
  config = {
    powerManagement.powerUpCommands = with pkgs; ''
      ${spindownAllHdds}/bin/spindownAllHdds
    '';
    home-manager.users.mhuber = {
      home.packages = [ spindownAllHdds ];
    };
  };
}

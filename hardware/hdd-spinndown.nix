{pkgs, ...}:
let
  spindownAllHdds = with pkgs; writeScriptBin "spindownAllHdds" ''
    rotHdds() {
      ${utillinux}/bin/lsblk -dnp -o name,rota |
          ${gnugrep}/bin/grep \'.*\\s1\' |
          ${coreutils}/bin/cut -d \' \' -f 1
    }
    ${hdparm}/bin/hdparm -S 240 -B 127 $(rotHdds)
  '';
in
{ config =
    { powerManagement.powerUpCommands = with pkgs;''
      ${spindownAllHdds}/bin/spindownAllHdds
      '';
    };
}

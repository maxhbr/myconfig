{pkgs, ...}:
{ config =
    { powerManagement.powerUpCommands = with pkgs;''
        ${bash}/bin/bash -c '${hdparm}/bin/hdparm -S 240 -B 127 $(${utillinux}/bin/lsblk -dnp -o name,rota |${gnugrep}/bin/grep \'.*\\s1\'|${coreutils}/bin/cut -d \' \' -f 1)'
      '';
    };
}

{ pkgs, ... }:
let
  spindownAllHdds =
    with pkgs;
    writeScriptBin "spindownAllHdds" ''
      rotHdds() {
        ${util-linux}/bin/lsblk -dnp -o name,rota |
            ${gnugrep}/bin/grep '.*[[:space:]]1' |
            ${coreutils}/bin/cut -d ' ' -f 1
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
    # Replace deprecated powerManagement.powerUpCommands (ran at boot and on
    # resume) with an explicit systemd oneshot that runs the same
    # spindownAllHdds command at the same lifecycle points.
    systemd.services.hdd-spindown = {
      description = "Spin down rotational HDDs (hdparm standby timer)";
      wantedBy = [
        "multi-user.target"
        "suspend.target"
        "hibernate.target"
        "hybrid-sleep.target"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${spindownAllHdds}/bin/spindownAllHdds";
        RemainAfterExit = true;
      };
    };
    home-manager.users.mhuber = {
      home.packages = [ spindownAllHdds ];
    };
  };
}

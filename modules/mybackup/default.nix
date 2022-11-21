# see also: https://xeiaso.net/blog/borg-backup-2021-01-09
{ config, lib, pkgs, ... }:
let

  myborgbackup-deps = with pkgs; [ borgbackup ];
  myborgbackup = pkgs.runCommandLocal "myborgbackup.sh" {
    nativeBuildInputs = [ pkgs.makeWrapper ];
  } ''
    install -m755 ${./myborgbackup.sh} -D $out/bin/myborgbackup.sh
    patchShebangs $out/bin/myborgbackup.sh
    wrapProgram "$out/bin/myborgbackup.sh" --prefix PATH : ${
      pkgs.lib.makeBinPath myborgbackup-deps
    }
  '';

  backupLuksHeader-deps = with pkgs; [ cryptsetup ];
  backupLuksHeader = pkgs.runCommandLocal "backupLuksHeader.sh" {
    nativeBuildInputs = [ pkgs.makeWrapper ];
  } ''
    install -m755 ${./backupLuksHeader.sh} -D $out/bin/backupLuksHeader.sh
    patchShebangs $out/bin/backupLuksHeader.sh
    wrapProgram "$out/bin/backupLuksHeader.sh" --prefix PATH : ${
      pkgs.lib.makeBinPath backupLuksHeader-deps
    }
  '';

in {
  config = {
    environment.systemPackages =
      [ pkgs.borgbackup myborgbackup backupLuksHeader ];
  };
}

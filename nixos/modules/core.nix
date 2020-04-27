{ config, pkgs, lib, ... }:
{
  config = {
    boot = {
      # kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
      cleanTmpDir = true;
      # tmpOnTmpfs = true;
      crashDump.enable = true;
    };

    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        taskwarrior
      ];
    };

    environment = {
      variables = {
        TMP = "/tmp";
      };
      systemPackages = with pkgs; [
        kbd

        # core:
        wget curl
        git git-lfs
        unzip
        tree
        rlwrap
        vim

        # admin:
        bind bridge-utils
        sysstat
        cryptsetup
        lsof
        psmisc # contains: killall, pstree
        lm_sensors

        nixops

        #others:
        rsnapshot
        mosh

        # my backup tool
        borgbackup
      ];
    };

    system.activationScripts.media = ''
      mkdir -m 0755 -p /media
    '';

    documentation.nixos.enable = true;
    services = {
      acpid.enable = true;
      ntp.enable = true;
      nscd.enable = true;
      earlyoom.enable = true;
    };

    programs.ssh.startAgent = true;
    programs.thefuck.enable = true;
    programs.firejail.enable = true;
    programs.mtr.enable = true;
  };
}

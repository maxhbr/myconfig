{ config, pkgs, lib, ... }:
{
  imports = [
    ./vim
    ./zsh
    ./tmux
    ./pass
    ./git
    ./myborgbackup
    ./nixos.networking
    ./nixos.nix.nix
    ./user.mhuber.nix
    ./dic.nix
  ];
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
      shellAliases = {
        upg = "~/myconfig/rebuild.sh";
        upg-dry = "~/myconfig/rebuild.sh --dry-run";
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

    services = {
      nixosManual.showManual = true;
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

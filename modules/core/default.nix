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
    ./nixos.nixpkgs-unstable
    ./user.mhuber.nix
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
        mtr bind bridge-utils
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
    };

    programs.ssh.startAgent = true;
  };
}

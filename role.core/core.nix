{ config, pkgs, lib, ... }: {
  config = {
    boot = {
      # kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
      # cleanTmpDir = ! config.boot.cleanTmpDir;
      tmpOnTmpfs = true;
    };

    home-manager.users.mhuber = {
      home.packages = with pkgs; [ taskwarrior mosh sshfs ];
      home.file = {
        ".ssh/config".text = ''
          ControlMaster auto
          ControlPath ~/.ssh/control:%h:%p:%r
          Include ~/.ssh/imports/*.config

          Host localhost
              StrictHostKeyChecking no
              UserKnownHostsFile=/dev/null

          Host 127.0.0.1
              StrictHostKeyChecking no
              UserKnownHostsFile=/dev/null
        '';
        ".ssh/imports/wireguard.config".text = ''
          Host 10.199.199.*
              User mhuber
        '';
      };
    };

    environment = {
      variables = { TMP = "/tmp"; };
      systemPackages = with pkgs; [
        kbd

        # core:
        wget
        curl
        git
        git-lfs
        unzip
        tree
        rlwrap
        vim

        # admin:
        bind
        bridge-utils
        sysstat
        cryptsetup
        lsof
        psmisc # contains: killall, pstree
        lm_sensors
        iperf

        #others:
        nfs-utils
        libnfs
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

    programs.bash.interactiveShellInit = ''
      umask 027
    '';
    programs.zsh.interactiveShellInit = ''
      umask 027
    '';
    programs.ssh.startAgent = true;
    programs.firejail.enable = true;
    programs.mtr.enable = true;
  };
}

{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  config = {
    boot = {
      # kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
      # cleanTmpDir = ! config.boot.cleanTmpDir;
      tmp.useTmpfs = true;
    };

    environment = {
      variables = {
        TMP = "/tmp";
      };
      systemPackages = with pkgs; [
        kbd

        # core:
        wget
        curl
        git
        git-lfs
        unzip
        p7zip
        tree # build fails?
        rlwrap
        vim

        comma

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
      nscd.enable = true;
      earlyoom.enable = true;
    };

    programs.bash.interactiveShellInit = ''
      umask 027
    '';
    programs.zsh.interactiveShellInit = ''
      umask 027
    '';
    programs.firejail.enable = true;
    programs.mtr.enable = true;
  };
}

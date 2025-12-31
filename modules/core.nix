{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  config = {
    environment = {
      # variables = {
      #   TMP = "/tmp";
      # };
      systemPackages = with pkgs; [
        bind
        borgbackup
        bridge-utils
        cryptsetup
        curl
        git
        git-lfs
        iperf
        kbd
        libnfs
        lm_sensors
        lsof
        nfs-utils
        p7zip
        psmisc # contains: killall, pstree
        rlwrap
        sysstat
        tree
        unzip
        vim
        wget
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
    programs.mtr.enable = true;
  };
}

{ config, pkgs, ... }:
let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee /etc/nixos/hostid
  hostId = "${builtins.readFile /etc/nixos/hostid}";
in {
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./core.nix
    ./mhuber.nix
    ./oh-my-zsh.nix
    ./userPackages.nix
  ]
  # the machine specific configuration is placed at ./machines/<hostName>.nix
    ++ (let
          path = (./machines + "/${hostName}.nix");
        in if builtins.pathExists path
           then [path]
           else [])
  # old configuration can be placed at /etc/nixos/configuration.old.nix
    ++ (if builtins.pathExists /etc/nixos/configuration.old.nix
        then [/etc/nixos/configuration.old.nix]
        else [])
  # all files in /etc/nixos/imports are sourced
    ++ (let
          path = /etc/nixos/imports;
        in if builtins.pathExists path
           then let
                  content = builtins.readDir path;
                in map (n: import (path + ("/" + n)))
                         (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                           (builtins.attrNames content))
           else [])
  # all files in ./misc are sourced
    ++ (let
          path = ./misc;
        in if builtins.pathExists path
           then let
                  content = builtins.readDir path;
                in map (n: import (path + ("/" + n)))
                         (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                           (builtins.attrNames content))
           else []);

  config = {
    networking.hostId = "${hostId}";
    networking.hostName = "${hostName}";
    system.copySystemConfiguration = true;

    nixpkgs = {
      config = import ../nix/nixpkgs-config.nix;
      overlays = import ../nix/nixpkgs-overlays.nix;
    };

    # option definitions
    boot = {
      # kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
      kernel.sysctl = {
        # "fs.inotify.max_user_watches" = 524288;
        "vm.swappiness" = 1;
      };
      # tmpOnTmpfs = true;
    };

    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };

    environment = {
      variables = {
        TMP = "/tmp";
      };
      interactiveShellInit = ''
        alias upg='~/myconfig/rebuild.sh'
      '';

      # shellInit = ''
      # '';
      # loginShellInit = ''
      # '';
      systemPackages = with pkgs; [
        kbd

        # core:
        wget curl
        git git-lfs
        unzip
        tree
        stow

        # cli:
        ranger
        vim
        tmux
        manpages
        ag
        file

        pass

        # admin:
        htop iftop iptraf-ng iotop bmon s-tui
        mtr bind bridge-utils
        pwgen # unstable.mkpasswd
        usbutils
        sysstat
        tcpdump
        cryptsetup
        lsof
        psmisc # contains: killall, pstree
        lm_sensors

        #others:
        pmount fuse
        rsnapshot
        borgbackup
      ];
    };

    nix = {
      useSandbox = true;
      readOnlyStore = true;

      binaryCachePublicKeys = [
        "hydra.snabb.co-1:zPzKSJ1mynGtYEVbUR0QVZf9TLcaygz/OyzHlWo5AMM=" # snabb.co
      ];
      trustedBinaryCaches = [
        "https://cache.nixos.org" "https://hydra.snabb.co"
      ];
      binaryCaches = [
        "https://cache.nixos.org" "https://hydra.snabb.co"
      ];

      extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';
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

    security = {
      sudo.extraConfig = ''
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl suspend
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl reboot
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl poweroff
      '';
      wrappers = {
        pmount.source  = "${pkgs.pmount}/bin/pmount";
        pumount.source  = "${pkgs.pmount}/bin/pumount";
      };
    };
    programs.ssh.startAgent = true;
  };
}

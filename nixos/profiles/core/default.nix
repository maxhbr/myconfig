{ config, pkgs, lib, ... }:

{
  imports = [
    ./oh-my-zsh.nix
    ./mhuber.nix
  ];

  boot = {
    # kernelPackages = pkgs.linuxPackages_testing;
    kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
    cleanTmpDir = true;
  };

  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = true;
      # allowedTCPPorts = [ 80 443 ];
      allowPing = false;
    };
  };

  environment = {
    # shellInit = ''
    # '';
    # loginShellInit = ''
    # '';
    systemPackages = with pkgs; [
      kbd
      coreEnv
      pmount fuse
      cryptsetup
      rsnapshot
      stow
    ];
  };

  nix = {
    useSandbox = true;
    readOnlyStore = true;
    buildCores = 4;

    binaryCachePublicKeys = [
       "hydra.cryp.to-1:8g6Hxvnp/O//5Q1bjjMTd5RO8ztTsG8DKPOAg9ANr2g=" # crypt.to
       "hydra.snabb.co-1:zPzKSJ1mynGtYEVbUR0QVZf9TLcaygz/OyzHlWo5AMM=" # snabb.co
       # "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" # reflex-frp
    ];
    trustedBinaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co" "http://hydra.cryp.to/"
      # "https://nixcache.reflex-frp.org"
    ];
    binaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co" "http://hydra.cryp.to/"
    ];

    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      auto-optimise-store = true
      binary-caches-parallel-connections = 10
    '';
  };

  system = {
    activationScripts.media = ''
      mkdir -m 0755 -p /media /share
    '';
  };

  services = {
    nixosManual.showManual = true;
    acpid.enable = true;
    ntp.enable = true;
    # emacs = {
    #   enable = true;
    #   # package = import /home/mhuber/.emacs.d { pkgs = pkgs; };
    # };
  };

  security.wrappers = {
    pmount.source  = "${pkgs.pmount}/bin/pmount";
    pumount.source  = "${pkgs.pmount}/bin/pumount";
  };
  programs.ssh.startAgent = true;
}

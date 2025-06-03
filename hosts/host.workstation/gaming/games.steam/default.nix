{ pkgs, config, lib, ... }:
let
  steam = pkgs.nixos-unstable.steam.override {
    extraPkgs = innerPkgs:
      with innerPkgs; [
        mono
        gtk3
        gtk3-x11
        libgdiplus
        zlib
        libffi
      ];
  };
in {
  imports = [
    (lib.mkIf false { # for sharing / viewing via steam
      networking.firewall = {
        # https://support.steampowered.com/kb_article.php?ref=8571-GLVN-8711&l=german
        allowedUDPPorts =
          [ 27031 27032 27033 27034 27035 27036 3478 4379 4380 ];
        allowedTCPPorts = [ 27036 27037 ];
      };
    })
  ];
  config = {
    # programs.firejail = {
    #   enable = true;
    #   wrappedBinaries = {
    #     steam = {
    #       executable = "${lib.getBin steam}/bin/steam";
    #       extraArgs = [
    #         "--net=enp39s0" "--dns=8.8.8.8" "--noprofile"
    #       ];
    #     };
    #   };
    # };
    home-manager.sharedModules = [{
      home.packages = [ steam ];
      myconfig.persistence.cache-directories = [ ".local/share/Steam" ];
      # home.file = {
      #   ".local/share/Steam/compatibilitytools.d/Proton-5.9-GE-2-MF" = {
      #     source = builtins.fetchTarball {
      #       url =
      #         "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/5.9-GE-2-MF/Proton-5.9-GE-2-MF.tar.gz";
      #       sha256 = "07chyahhkzl7d4p8pig43p1x6y8387pyjdlr275n0qha35jj2s8f";
      #     };
      #     recursive = true;
      #   };
      #   ".local/share/Steam/compatibilitytools.d/Proton-5.9-GE-5-ST" = {
      #     source = builtins.fetchTarball {
      #       url =
      #         "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/5.9-GE-5-ST/Proton-5.9-GE-5-ST.tar.gz";
      #       sha256 = "0zldsmanxjl6s9dii0ccfyp1i1lnk208jmixkvhd6y3yq1x7yy61";
      #     };
      #     recursive = true;
      #   };
      # };
    }];

    programs.steam.enable = true;

    hardware = {
      opengl = {
        # driSupport = true;
        driSupport32Bit = true;
      };
      pulseaudio.support32Bit = true;
    };
  };
}

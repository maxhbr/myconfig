{ pkgs, lib, ... }:
let
  nativeOnly = false;
in {
  imports = [
    ./steamcontroller.nix
    (lib.mkIf nativeOnly { nixpkgs.config.allowBroken = true; })
  ];
  config = {
    home-manager.users.mhuber =
      { home.packages =
          with pkgs;
          [ (steam.override { inherit nativeOnly; })
            (if nativeOnly then steam-run-native else steam-run)
          ];
        home.file =
          { ".local/share/Steam/compatibilitytools.d/Proton-5.6-GE-2" =
              { source = builtins.fetchTarball
                  { url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/5.6-GE-2/Proton-5.6-GE-2.tar.gz";
                    sha256 = "05gk6c0wkn88d8a1ivhl4g53mq16j63rhr3hwnhwhphjk8b1h8yj";
                  };
                recursive = true;
              };
            ".local/share/Steam/compatibilitytools.d/Proton-5.9-GE-2-MF" =
              { source = builtins.fetchTarball
                  { url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/5.9-GE-2-MF/Proton-5.9-GE-2-MF.tar.gz";
                    sha256 = "07chyahhkzl7d4p8pig43p1x6y8387pyjdlr275n0qha35jj2s8f";
                  };
                recursive = true;
              };
          };
      };

    hardware= {
      opengl = {
        driSupport = true;
        driSupport32Bit = true;
      };
      pulseaudio.support32Bit = true;
    };
    # for sharing / viewing via steam
    networking.firewall = {
      allowedUDPPorts = [ 27031 27036 ];
      allowedTCPPorts = [ 27036 27037 ];
    };
  };
}

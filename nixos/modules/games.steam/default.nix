{ pkgs, ... }:
{
  imports = [
    ./steamcontroller.nix
  ];
  config = {
    home-manager.users.mhuber =
      { home.packages = with pkgs; [
          steam steam-run
        ];
        home.file =
          { ".local/share/Steam/compatibilitytools.d/Proton-5.6-GE-2" =
              { source = builtins.fetchTarball
                  { url = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/5.6-GE-2/Proton-5.6-GE-2.tar.gz";
                    sha256 = "05gk6c0wkn88d8a1ivhl4g53mq16j63rhr3hwnhwhphjk8b1h8yj";
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

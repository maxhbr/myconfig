{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.dev.alpaca = {
      enable = mkEnableOption "myconfig.ai.dev.alpaca";
    };
  };
  config = lib.mkIf config.myconfig.ai.dev.alpaca.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [ alpaca ];
        myconfig.persistence.files = [ ".local/share/com.jeffser.Alpaca/alpaca.db" ];
      }
    ];
  };
}

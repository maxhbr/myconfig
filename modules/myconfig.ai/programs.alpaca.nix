{ config, lib, pkgs, ... }: {
  options.myconfig = with lib; {
    ai.alpaca = { enable = mkEnableOption "myconfig.ai.alpaca"; };
  };
  config = lib.mkIf config.myconfig.ai.alpaca.enable {
    home-manager.sharedModules = [{ 
      home.packages = with pkgs; [ alpaca ]; 
      myconfig.persistence.files = [
        ".local/share/com.jeffser.Alpaca/alpaca.db"
      ];
    }];
  };
}


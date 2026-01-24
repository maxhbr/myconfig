{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.coding.enable = mkEnableOption "myconfig.ai.coding ";
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.coding.enable) {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          # aider-chat
          code-cursor
          zed
        ];
        # myconfig.persistence.cache-files = [
        #   ".local/share/opencode/auth.json"
        # ];
        myconfig.persistence.cache-directories = [
          ".config/Cursor"
          ".cursor"
        ];
        myconfig.desktop.wayland.launcherCommands = [ "cursor" ];
      }
    ];
    myconfig.ai.opencode.enable = lib.mkDefault true;
    myconfig.ai.codex.enable = lib.mkDefault true;
    myconfig.ai.qwen-code.enable = lib.mkDefault true;
    myconfig.ai.claude-code.enable = lib.mkDefault true;
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig.ai.services = with lib; {
    llama-server = {
      enable = mkEnableOption "myconfig.ai.services.llama-server";
    };
  };

  config = lib.mkIf config.myconfig.ai.services.llama-server.enable {
    home-manager.sharedModules = [
      {
        systemd.user.services.llama-server = {
          Unit = {
            Description = "Local LLM inference server (llama-server)";
            Wants = [ "graphical-session.target" ];
            After = [ "graphical-session.target" ];
            PartOf = [ "graphical-session.target" ];
            Requisite = [ "graphical-session.target" ];
          };
          Service = {
            ExecStart = "${pkgs.llama-cpp}/bin/llama-server -m ~/MINE/models/GLM-4.7-Flash-BF16.gguf --port 22545";
            Restart = "on-failure";
            Environment = [
              "HSA_OVERRIDE_GFX_VERSION=11.5.1"
              "HIP_VISIBLE_DEVICES=1"
              "HCC_AMDGPU_TARGET=gfx1151"
              "HSA_ENABLE_SDMA=1"
            ];
          };
        };
      }
    ];
  };
}

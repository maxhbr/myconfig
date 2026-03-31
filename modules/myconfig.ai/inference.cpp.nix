{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.inference-cpp;
  gpuvariants = config.myconfig.hardware.gpu.variant;
  hasVariant = v: builtins.elem v gpuvariants;
  matching-llama-cpp =
    if hasVariant "amd" then
      pkgs.llama-cpp-rocm
    else if hasVariant "amd-no-rocm" then
      pkgs.llama-cpp-vulkan
    # else  hasVariant "nvidia" then
    #   pkgs.llama-cpp.override { cudaSupport = true; }
    else
      pkgs.llama-cpp;
in
{
  options.myconfig = with lib; {
    ai.inference-cpp = {
      enable = mkEnableOption "myconfig.ai.inference-cpp";
      llama-cpp.package = mkOption {
        type = types.package;
        default = matching-llama-cpp;
        description = "The llama-cpp package to use";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.llama-cpp.package = cfg.llama-cpp.package;
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          # koboldcpp
          cfg.llama-cpp.package
        ];
        myconfig.persistence.cache-directories = [ ".cache/llama.cpp/" ];
      }
    ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  gpuvariant = config.myconfig.hardware.gpu.variant;
  matching-llama-cpp =
    if gpuvariant == "amd" then
      llama-cpp-rocm
    else if gpuvariant == "amd-no-rocm" then
      llama-cpp-vulcan
    else
      llama-cpp;
in
{
  options.myconfig = with lib; {
    ai.inference-cpp = {
      enable = mkEnableOption "myconfig.ai.inference-cpp";
    };
  };
  config = lib.mkIf config.myconfig.ai.inference-cpp.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          koboldcpp
          matching-llama-cpp
        ];
      }
    ];
  };
}

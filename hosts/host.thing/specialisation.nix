{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    (
      {
        lib,
        config,
        pkgs,
        ...
      }:
      {
        config = lib.mkIf (config.specialisation != { }) {
          # Config that should only apply to the default system, not the specialised ones
        };
      }
    )
  ];
  specialisation = {
    # qwen-35-122 = {
    #   inheritParentConfig = true;
    #   configuration = {
    #     services.llama-cpp = {
    #       enable = true;
    #       model = "/mnt/disk/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf";
    #       port = 22545;
    #       extraFlags = [
    #         "-fa"
    #         "on"
    #       ];
    #     };
    #     systemd.services.llama-cpp = {
    #       environment = {
    #         LLAMA_ARG_DEVICE = "Vulkan1";
    #       };
    #     };
    #   };
    # };
  };
}

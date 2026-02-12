{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    myconfig.hardware.gpu.variant = "amd";
  };
}

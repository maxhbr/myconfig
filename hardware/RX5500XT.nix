{
  pkgs,
  config,
  inputs,
  ...
}:
{
  config = {
    myconfig.hardware.gpu.variant = "amd";
  };
}

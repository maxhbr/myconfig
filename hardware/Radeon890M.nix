{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    myconfig.hardware.gpu.variant = "amd-no-rocm"; # gfx1150 has no rocm support, see https://rocm.docs.amd.com/en/latest/compatibility/compatibility-matrix.html
  };
}

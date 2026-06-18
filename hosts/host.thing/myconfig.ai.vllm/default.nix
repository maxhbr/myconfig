{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./docker.vllm.nix
  ];
}

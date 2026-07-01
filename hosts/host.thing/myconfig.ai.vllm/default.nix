{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./docker.vllm.cuda.nix
    ./docker.vllm.rocm.nix
  ];
}

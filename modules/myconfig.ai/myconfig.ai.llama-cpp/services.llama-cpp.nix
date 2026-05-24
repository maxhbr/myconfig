# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Picks the right llama-cpp package for the host's GPU variant and exposes
# it via:
#   - `myconfig.ai.inference-cpp.llama-cpp.package` (override hook)
#   - `services.llama-cpp.package` (the upstream nixpkgs module's package)
#   - the user's `home.packages` (so `llama-server` / `llama-bench` etc.
#     are on $PATH for ad-hoc use)
#
# This is the "single binary on the host" path, distinct from the
# llama-swap-orchestrated per-(model, device) wrappers in ./llama-swap.nix.
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.inference-cpp;
  # `myconfig.hardware.gpu.variant` is not always imported (e.g. inside
  # the containers that import this module to pick up the
  # `myconfig.ai.llama-cpp.models` option). Fall back to "no variants"
  # there so option defaults can still evaluate without errors. When the
  # option is absent the module is also `enable = false`, so the chosen
  # package never actually gets used.
  gpuvariants =
    if lib.hasAttrByPath [ "myconfig" "hardware" "gpu" "variant" ] options then
      config.myconfig.hardware.gpu.variant
    else
      [ ];
  hasVariant = v: builtins.elem v gpuvariants;
  my-llama-cpp = pkgs.llama-cpp.override {
    rocmSupport = hasVariant "amd";
    vulkanSupport = (hasVariant "amd-no-rocm" || hasVariant "amd");
    cudaSupport = hasVariant "nvidia";
    blasSupport = false;
  };
  hmEnabled = lib.hasAttrByPath [ "home-manager" "sharedModules" ] options;
in
{
  options.myconfig = with lib; {
    ai.inference-cpp = {
      enable = mkEnableOption "myconfig.ai.inference-cpp";
      llama-cpp.package = mkOption {
        type = types.package;
        default = my-llama-cpp;
        description = "The llama-cpp package to use";
      };
    };
  };
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        services.llama-cpp.package = lib.mkDefault cfg.llama-cpp.package;
      }
      # home-manager is not present in NixOS container configs, which now also
      # import this module to pick up `myconfig.ai.llama-cpp.models`. Guard the
      # home-manager bits so they only apply where the option actually exists.
      (lib.optionalAttrs hmEnabled {
        home-manager.sharedModules = [
          {
            home.packages = with pkgs; [
              cfg.llama-cpp.package
            ];
            myconfig.persistence.cache-directories = [ ".cache/llama.cpp/" ];
          }
        ];
      })
    ]
  );
}

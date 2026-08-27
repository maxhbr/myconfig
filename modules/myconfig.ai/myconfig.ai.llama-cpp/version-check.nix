# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Exposes the resolved llama.cpp version for each GPU backend the host
# actually runs, and asserts they agree, so a future overlay that pins only
# ONE of {llama-cpp, llama-cpp-vulkan, llama-cpp-rocm} cannot silently leave
# the others on a different (older) engine.
#
# Why this is needed: the llama-swap launchers (lib/scripts.nix ->
# devices.llamaServerFor) select `pkgs.llama-cpp-vulkan` / `pkgs.llama-cpp-rocm`
# directly, NOT the host's `services.llama-cpp.package`. A host overlay that
# only overrides `pkgs.llama-cpp` would therefore have to reach those
# attributes via `overrideAttrs`+`override` composition. This was verified to
# compose (see hosts/host.thing/nixpkgs.overlays.llama-cpp.nix); this module
# keeps that property detectable.
#
# A per-model fork package (e.g. the Nathanw1014 strix-halo-vulkan build used
# by the DFlash2 candidate) is intentionally NOT covered here — it is a
# separate derivation with its own version, surfaced via each candidate's
# `serverPackage`.
{
  config,
  lib,
  pkgs,
  options,
  ...
}:
let
  # Upstream (non-fork) llama-cpp packages the host can reach and that a
  # host overlay must keep in sync. `cuda` is built ad-hoc in lib/devices.nix
  # as `pkgs.llama-cpp.override { cudaSupport = true; }`, so its version
  # always tracks `base` by construction (override composes with the
  # overlay's overrideAttrs); it is intentionally NOT forced here to avoid
  # pulling cudaPackages into AMD-only container evals.
  upstreamBackends = {
    base = pkgs.llama-cpp;
    vulkan = pkgs.llama-cpp-vulkan;
    rocm = pkgs.llama-cpp-rocm;
  };

  backendInfo = name: pkg: {
    inherit name;
    version = pkg.version or null;
    # src.rev / src.tag come from the fetcher and are available at eval time
    # (no build needed).
    srcRev = pkg.src.rev or null;
    srcTag = pkg.src.tag or null;
    outPath = pkg.outPath;
  };

  versions = lib.mapAttrs backendInfo upstreamBackends;
  versionValues = lib.attrValues (lib.mapAttrs (_: p: p.version) upstreamBackends);
  headVersion = builtins.head versionValues;
  allSameVersion = lib.all (v: v == headVersion) versionValues;

  # Pretty-print the per-backend versions for the assertion message so a
  # drift is immediately diagnosable.
  versionsRepr = lib.concatStringsSep ", " (
    lib.mapAttrsToList (n: v: "${n}=${v}") (lib.mapAttrs (_: p: p.version) upstreamBackends)
  );
in
{
  options.myconfig.ai.llama-cpp.backendVersions = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            readOnly = true;
            description = "Backend key (base/vulkan/rocm/cuda).";
          };
          version = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            readOnly = true;
            description = "llama.cpp version string the backend was built from.";
          };
          srcRev = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            readOnly = true;
            description = "Resolved source rev (git tag/commit) the backend was built from.";
          };
          srcTag = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            readOnly = true;
            description = "Source tag the backend was built from, if fetched by tag.";
          };
          outPath = lib.mkOption {
            type = lib.types.str;
            readOnly = true;
            description = "Store path of the backend package (changes with the pin).";
          };
        };
      }
    );
    readOnly = true;
    description = ''
      Resolved llama.cpp version for each GPU backend reachable from the
      host's `pkgs`, after the host's `nixpkgs.overlays` are applied. Use
      `nix eval .#nixosConfigurations.<host>.config.myconfig.ai.llama-cpp.backendVersions`
      to inspect the pinned versions at eval time.
    '';
    default = versions;
  };

  config.assertions = [
    {
      assertion = allSameVersion;
      message = ''
        myconfig.ai.llama-cpp: the upstream llama.cpp backends drifted to
        different versions (${versionsRepr}). A host overlay that pins
        llama.cpp must reach llama-cpp-vulkan AND llama-cpp-rocm (and the
        ad-hoc cuda override) with the same revision; otherwise the
        llama-swap launchers (which select pkgs.llama-cpp-vulkan /
        pkgs.llama-cpp-rocm directly) would run a different engine than the
        one the overlay intended. See
        hosts/host.thing/nixpkgs.overlays.llama-cpp.nix and
        modules/myconfig.ai/myconfig.ai.llama-cpp/version-check.nix.
      '';
    }
  ];
}

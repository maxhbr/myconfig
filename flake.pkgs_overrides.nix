# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# ============================================================================
# Local Package Overrides (upstream nixpkgs bug workarounds)
# ============================================================================
#
# This module applies overlays that patch packages in nixpkgs to work around
# upstream bugs that have not yet been fixed or merged. Each override has a
# comment explaining the bug and when it can be removed.
#
# All overrides below are exposed by the recent nixpkgs bump to
# Python 3.14 + pandas 3.0.4 + setuptools 82.
#
# | Package        | Problem                                                       |
# | -------------- | ------------------------------------------------------------- |
# | dfdiskcache    | Upstream metadata pins `pandas<3,>=1`; nixpkgs now ships     |
# |                | pandas 3.x, so `pythonRuntimeDepsCheckHook` rejects the      |
# |                | build. Relaxed via `pythonRelaxDeps`. Breaks the             |
# |                | sbomnix -> dfdiskcache -> pandas build chain.                 |
# | voxtype-vulkan | nixpkgs builds without any `osd-*` cargo feature, so neither   |
# |                | `voxtype-osd-gtk4` nor `voxtype-osd-native` lands on PATH and   |
# |                | the `voxtype-osd` launcher crashes on every daemon start.    |
# |                | Override builds the `osd-gtk4` feature + GTK4 deps.           |
#
# When removing an override, also drop its entry here and rebuild.

{ }:
{
  ...
}:

{
  nixpkgs.overlays = [
    # dfdiskcache: upstream metadata declares `pandas<3,>=1`, but nixpkgs now
    # ships pandas 3.0.4, so `pythonRuntimeDepsCheckHook` rejects the build
    # (and sbomnix, which depends on dfdiskcache, fails too).
    #
    # `pythonRelaxDeps = [ "pandas" ]` rewrites the wheel's `Requires-Dist`
    # from `pandas<3,>=1` to `pandas` so the runtime-deps check passes.
    # df-diskcache works fine with pandas 3.x (it only uses DataFrame caching).
    #
    # NOTE: this is applied via `pythonPackagesExtensions` (not
    # `python3.override`), because the top-level `python3Packages` attribute
    # derives from `python314` (via `python314Packages`), NOT from the
    # `python3` attribute. Overriding `python3` alone leaves
    # `python3Packages.dfdiskcache` pointing at the unpatched derivation, so
    # sbomnix (and anything else consuming `python3Packages.dfdiskcache`)
    # keeps failing. `pythonPackagesExtensions` is applied to *every* python
    # package set's scope, so it covers `python314Packages` too.
    #
    # TODO: remove once upstream df-diskcache releases a version allowing
    # pandas 3.x and nixpkgs picks it up.
    (_final: prev: {
      pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
        (_pyfinal: pyprev: {
          dfdiskcache = pyprev.dfdiskcache.overridePythonAttrs (_old: {
            pythonRelaxDeps = [ "pandas" ];
          });
        })
      ];
    })

    # voxtype-vulkan: build the optional `osd-gtk4` cargo feature so the GTK4
    # OSD binary (`voxtype-osd-gtk4`) ships with the package. Upstream nixpkgs
    # builds voxtype with no OSD feature enabled, so the always-built
    # `voxtype-osd` launcher fails with:
    #   voxtype-osd: neither 'voxtype-osd-native' nor 'voxtype-osd-gtk4'
    #   was found on PATH or next to this binary.
    # …on every daemon start, then gives up after 3 retries.
    #
    # `voxtype-vulkan` is a separate top-level attribute
    # (callPackage ... { vulkanSupport = true; }), so the override must target
    # it directly, not `voxtype`. `overrideAttrs` targets `cargoBuildFeatures`
    # (the derivation attr the cargo-build-hook actually reads — `buildFeatures`
    # is only an input to buildRustPackage's flag computation, which already
    # ran) and appends the GTK4 runtime libs to `buildInputs`. The optional deps
    # are already pinned in Cargo.lock, so `cargoHash` is unchanged.
    #
    # Upstream issue: https://github.com/NixOS/nixpkgs/issues/533080
    #
    # TODO: remove once nixpkgs enables `osd-gtk4` (or `osd-native`) in
    # pkgs/by-name/vo/voxtype/package.nix.
    (_final: prev: {
      voxtype-vulkan = prev.voxtype-vulkan.overrideAttrs (old: {
        cargoBuildFeatures = (old.cargoBuildFeatures or [ ]) ++ [ "osd-gtk4" ];
        cargoCheckFeatures = (old.cargoCheckFeatures or old.cargoBuildFeatures or [ ]) ++ [ "osd-gtk4" ];
        buildInputs = (old.buildInputs or [ ]) ++ [
          prev.gtk4
          prev.gtk4-layer-shell
          prev.cairo
          prev.glib
        ];
      });
    })
  ];
}

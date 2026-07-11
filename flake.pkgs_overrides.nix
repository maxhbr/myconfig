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
# | input-remapper | (a) Missing `packaging` runtime dep; `versionCheckHook`      |
# |                |     crashes with `ModuleNotFoundError: No module named       |
# |                |     'packaging'` (setuptools 82 no longer re-exports it).     |
# |                | (b) Top-level `import pkg_resources` in `configs/data.py`     |
# |                |     crashes the whole module, because setuptools 82 removed  |
# |                |     `pkg_resources`. Patched to import it lazily inside the   |
# |                |     already-existing try/except fallback.                     |
# | dfdiskcache    | Upstream metadata pins `pandas<3,>=1`; nixpkgs now ships     |
# |                | pandas 3.x, so `pythonRuntimeDepsCheckHook` rejects the      |
# |                | build. Relaxed via `pythonRelaxDeps`. Breaks the             |
# |                | sbomnix -> dfdiskcache -> pandas build chain.                 |
#
# When removing an override, also drop its entry here and rebuild.

{ }:
{
  ...
}:

{
  nixpkgs.overlays = [
    # input-remapper: (a) add missing `packaging` runtime dependency and
    # (b) patch `configs/data.py` so the top-level `import pkg_resources`
    # (removed in setuptools 82) becomes a lazy import inside the existing
    # try/except fallback in `_try_python_package_location`.
    #
    # Upstream `inputremapper/configs/migrations.py` does
    #   from packaging import version
    # and `configs/data.py` does
    #   import pkg_resources
    # but the nixpkgs `package.nix` `dependencies` list omits `packaging`,
    # and setuptools 82 (Python 3.14) no longer ships `pkg_resources`.
    #
    # TODO: remove once nixpkgs adds `packaging` to input-remapper deps and
    # patches the `pkg_resources` import (or upstream input-remapper releases
    # a version that drops `pkg_resources`).
    (_final: prev: {
      input-remapper = prev.input-remapper.overridePythonAttrs (old: {
        dependencies = (old.dependencies or [ ]) ++ [
          prev.python3Packages.packaging
        ];
        patches = (old.patches or [ ]) ++ [
          ./flake.pkgs_overrides.input-remapper-pkg-resources.patch
        ];
      });
    })

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
  ];
}

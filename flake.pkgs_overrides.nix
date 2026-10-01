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
# | herdr          | Fails to link at the pinned rev: `ld.bfd: .eh_frame_hdr refers  |
# |                | to overlapping FDEs` when linking the auditable-build object    |
# |                | against the zig-built `libghostty-vt`. Fixed upstream by       |
# |                | disabling `bundle_compiler_rt`/`bundle_ubsan_rt` in the        |
# |                | vendored zig build. Replaced wholesale with the build from the |
# |                | `master` channel, which contains that fix and is binary-cached.|
# | eternal-       | Fails to compile at the pinned rev against `abseil-cpp_202608` |
# | terminal       | + `protobuf 36`: abseil's installed headers require C++20        |
# |                | (`std::partial_ordering` et al.), but ET's `CMakeLists.txt`    |
# |                | hard-codes `set(CMAKE_CXX_STANDARD 17)`. Patched to 20.        |
# | dfdiskcache    | Upstream metadata pins `pandas<3,>=1`; nixpkgs now ships     |
# |                | pandas 3.x, so `pythonRuntimeDepsCheckHook` rejects the      |
# |                | build. Relaxed via `pythonRelaxDeps`. Breaks the             |
# |                | sbomnix -> dfdiskcache -> pandas build chain.                 |
# | pass-secret-   | Upstream calls `asyncio.get_event_loop()` in its entry point, |
# | service        | which since Python 3.14 raises `RuntimeError: There is no    |
# |                | current event loop` instead of implicitly creating one. The   |
# |                | pinned nixpkgs builds the package with `python3` (= python314),|
# |                | so every `services.pass-secret-service` user unit crashes on  |
# |                | startup. Override bumps src to the upstream fix commit.       |
# | voxtype-onnx   | nixpkgs builds without any `osd-*` cargo feature, so neither   |
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

    # voxtype-onnx: build the optional `osd-gtk4` cargo feature so the GTK4
    # OSD binary (`voxtype-osd-gtk4`) ships with the package. Upstream nixpkgs
    # builds voxtype with no OSD feature enabled, so the always-built
    # `voxtype-osd` launcher fails with:
    #   voxtype-osd: neither 'voxtype-osd-native' nor 'voxtype-osd-gtk4'
    #   was found on PATH or next to this binary.
    # …on every daemon start, then gives up after 3 retries.
    #
    # `voxtype-onnx` is a separate top-level attribute
    # (callPackage ... { onnxSupport = true; }), so the override must target
    # it directly, not `voxtype`. `overrideAttrs` targets `cargoBuildFeatures`
    # (the derivation attr the cargo-build-hook actually reads — `buildFeatures`
    # is only an input to buildRustPackage's flag computation, which already
    # ran) and appends the GTK4 runtime libs to `buildInputs`. The optional deps
    # are already pinned in Cargo.lock, so `cargoHash` is unchanged.
    #
    # (The same override previously targeted `voxtype-vulkan`; that variant
    # is no longer used since the voxtype config switched from Whisper to
    # Parakeet, so only the onnx variant is patched now.)
    #
    # Upstream issue: https://github.com/NixOS/nixpkgs/issues/533080
    #
    # TODO: remove once nixpkgs enables `osd-gtk4` (or `osd-native`) in
    # pkgs/by-name/vo/voxtype/package.nix.
    (_final: prev: {
      voxtype-onnx = prev.voxtype-onnx.overrideAttrs (old: {
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

    # pass-secret-service: crashes on startup under Python 3.14 because the
    # entry point `pass_secret_service.pass_secret_service._main()` calls
    # `asyncio.get_event_loop()`. Since Python 3.14 that call raises
    #   RuntimeError: There is no current event loop in thread 'MainThread'.
    # instead of implicitly creating a loop (the implicit-creation behaviour
    # was deprecated in 3.10 and made an error in 3.12+). The pinned nixpkgs
    # builds the package against `python3` (= python314), so every
    # `services.pass-secret-service` user unit fails with exit-code 1 on
    # every host that enables it (f13, workstation, ...).
    #
    # Upstream fix: mdellweg/pass_secret_service@b88e7ba (merged as PR #44,
    # commit 4ba0f4b) replaces the call with
    # `asyncio.new_event_loop()` + `asyncio.set_event_loop(mainloop)`.
    # Between the pinned rev (6335c85) and 4ba0f4b this is the *only* diff,
    # so bumping `src` is equivalent to patching the one line. nixpkgs master
    # made the same bump (pass-secret-service 0-unstable-2026-07-15) but the
    # flake's nixpkgs input predates it, hence this override.
    #
    # Because the override sets `src`/`version` to exactly what a future
    # nixpkgs already ships, it degrades to a no-op once the input is bumped
    # past nixpkgs commit 89d71ccd4648 (rather than a patch that would fail
    # to apply against the already-fixed source).
    #
    # TODO: remove once the nixpkgs input includes commit 89d71ccd4648
    # (pass-secret-service 0-unstable-2026-07-15).
    (_final: prev: {
      pass-secret-service = prev.pass-secret-service.overrideAttrs (old: {
        version = "0-unstable-2026-07-15";
        src = prev.fetchFromGitHub {
          owner = "mdellweg";
          repo = "pass_secret_service";
          rev = "4ba0f4b6c0667192263c385c13b5ec42a87af9ff";
          hash = "sha256-BaAULmeTxsj6uk3Aqe7ft/uN14M/b1U3ga7S71+lE68=";
        };
      });
    })

    # herdr: at the pinned nixpkgs the link of the auditable build fails:
    #   binutils-2.46/bin/ld.bfd: .eh_frame_hdr refers to overlapping FDEs
    #   binutils-2.46/bin/ld.bfd: final link failed: bad value
    # when linking against the static `libghostty-vt` produced by the
    # vendored zig build. Upstream fixed this (nixpkgs master, present in
    # `nixos-unstable-small` too) by patching
    # `vendor/libghostty-vt/src/build/GhosttyLibVt.zig` to set
    # `lib.bundle_compiler_rt = false;` and `lib.bundle_ubsan_rt = false;`.
    # Instead of re-implementing that patch here, take the whole package
    # from the `master` channel (exposed as `pkgs.master` by the
    # `mkSubPkgsOverlay`s in flake.nix). The resulting derivation
    # (/nix/store/wsaf3skg…-herdr-0.9.1) is binary-cached on
    # cache.nixos.org, so this costs no local build at all.
    #
    # TODO: remove once the nixpkgs input is bumped past the commit that
    # added the `postPatch` to pkgs/by-name/he/herdr/package.nix
    # (the drv then coincides with `master`'s and the override is a no-op).
    (self: _super: {
      herdr = self.master.herdr;
    })

    # eternal-terminal: fails to compile at the pinned nixpkgs against the
    # `abseil-cpp_20260817.0` + `protobuf 36.2` toolchain:
    #   absl/types/compare.h:60:12: error: 'partial_ordering' has not been
    #     declared in 'std'
    #   absl/container/btree_map.h:432:15: error: 'contains' has not been
    #     declared ...
    # abseil's installed headers require C++20 (`std::partial_ordering`,
    # `std::three_way_comparable`), but ET's `CMakeLists.txt` hard-codes
    # `set(CMAKE_CXX_STANDARD 17)` (with `CMAKE_CXX_STANDARD_REQUIRED ON`),
    # so a `CXXFLAGS`-based override is ignored. The expression is byte-
    # identical on all newer channels, so bumping the input does not help;
    # patch the standard to 20 instead. abseil LTS 202608 is built with and
    # expects C++20, so this matches what protobuf already compiles with.
    #
    # `doCheck = false` and `enableParallelBuilding = false` are needed for
    # the sandbox the f13 build runs in (`/run/mysbx-nix/state/builds`):
    # - the integration tests spawn an et daemon that `chown`s its socket
    #   path, which fails with `Error: (22): Invalid argument` there;
    # - at `-j24` the C++20 PCH makes parallel `cc1plus` processes OOM the
    #   build sandbox (`Killed signal terminated program cc1plus`).
    #
    # TODO: remove once upstream (EternalTerminal or nixpkgs) raises the
    # C++ standard to 20 (or an abseil/protobuf pairing compatible with
    # C++17 is restored) in a revision the nixpkgs input picks up; then
    # also restore the check phase on a builder that supports it.
    (_self: prev: {
      eternal-terminal = prev.eternal-terminal.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace CMakeLists.txt \
            --replace-fail "set(CMAKE_CXX_STANDARD 17)" "set(CMAKE_CXX_STANDARD 20)"
        '';
        doCheck = false;
        enableParallelBuilding = false;
      });
    })
  ];
}

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
# | eternal-       | Fails to compile at the pinned rev against `abseil-cpp_202608` |
# | terminal       | + `protobuf 36`: abseil's installed headers require C++20        |
# |                | (`std::partial_ordering` et al.), but ET's `CMakeLists.txt`    |
# |                | hard-codes `set(CMAKE_CXX_STANDARD 17)`. Patched to 20.        |
# | dfdiskcache    | Upstream metadata pins `pandas<3,>=1`; nixpkgs now ships     |
# |                | pandas 3.x, so `pythonRuntimeDepsCheckHook` rejects the      |
# |                | build. Relaxed via `pythonRelaxDeps`. Breaks the             |
# |                | sbomnix -> dfdiskcache -> pandas build chain.                 |
# | sbomnix        | Two deps fail at the pinned rev: `reuse` misses its `man`      |
# |                | output (sphinx-hook reads `sphinxBuilders` as a scalar under  |
# |                | `__structuredAttrs`), `sqliteschema` rejects `typepy` 2.0.0.  |
# |                | Taken from the `nixpkgs-sbomnix` input (binary-cached).       |
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
# | neovide        | 0.17.1 bump clobbered `src.hash` and `cargoHash` with the     |
# |                | skia-source hash, so the "neovide" fetch stores skia's tree |
# |                | (no Cargo.lock) and the vendor-staging drv fails. Taken from |
# |                | the `master` input, which has the corrected hashes.           |
# | aichat,        | nixpkgs 44cddf92c432 ("rustPlatform.cargoSetupHook: Force     |
# | ccusage,       | system libs") makes cargo build scripts link system libs via  |
# | codex, nono,   | pkg-config instead of vendored ones; these Rust packages lack   |
# | wprs           | the corresponding `buildInputs` and fail at build time.        |
# | waybar         | 0.16.0 regressed label centering: custom/* modules and niri/    |
# |                | workspaces text hugs the module's start edge and CSS          |
# |                | backgrounds cover only the glyph. Patched until fixed          |
# |                | upstream.                                                      |
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

    # sbomnix: at the pinned nixpkgs two of its dependencies fail to build
    # (`python3.14-reuse` misses its `man` output, `python3.14-sqliteschema`
    # rejects `typepy` 2.0.0), and master/nixos-unstable-small are broken the
    # same way. Take it from the `nixpkgs-sbomnix` input instead, whose whole
    # sbomnix closure is binary-cached. See
    # doc/TODOs/drop-sbomnix-nixpkgs-pin.md.
    (self: _super: {
      sbomnix = self.nixpkgs-sbomnix.sbomnix;
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

    # eternal-terminal: the C++17→C++20 bump (needed against the
    # `abseil-cpp_20260817.0` + `protobuf 36.2` toolchain) is now shipped
    # upstream via the package's `cmake-cxx-version.patch` and
    # `thread-pool-c++20-result-of.patch`, so no patching is needed here.
    #
    # `doCheck = false` and `enableParallelBuilding = false` are still needed
    # for the sandbox the f13 build runs in (`/run/mysbx-nix/state/builds`):
    # - the integration tests spawn an et daemon that `chown`s its socket
    #   path, which fails with `Error: (22): Invalid argument` there;
    # - at `-j24` the C++20 PCH makes parallel `cc1plus` processes OOM the
    #   build sandbox (`Killed signal terminated program cc1plus`).
    #
    # TODO: remove once the build sandbox supports the chown-based
    # integration tests and the PCH memory footprint.
    (_self: prev: {
      eternal-terminal = prev.eternal-terminal.overrideAttrs (_old: {
        doCheck = false;
        enableParallelBuilding = false;
      });
    })

    # neovide: the 0.17.1 bump (nixpkgs 1aeb44c24e, 2026-11-04) clobbered
    # `src.hash` and `cargoHash` with the *skia* source hash
    # (sha256-ahqSduR4…). Because a fixed-output fetch is keyed by its hash,
    # the "neovide" src fetch collapses onto the skia store path, so the
    # build unpacks skia's tree (no Cargo.lock) and fails at
    # `fetch-cargo-vendor-util create-vendor-staging`:
    #   FileNotFoundError: [Errno 2] No such file or directory: 'Cargo.lock'
    # dragging every neovide-dependent closure (f13 home-manager, toplevel)
    # into "Cannot build … dependency failed" cascades.
    #
    # Upstream fix: nixpkgs 2cc8df2292 ("neovide: fix source and cargo
    # hashes", 2026-11-05) — on master, not yet in the pinned
    # nixos-unstable branch head when this input was locked.
    #
    # See doc/TODOs/drop-neovide-master-pin.md.
    #
    # TODO: remove once the nixos-unstable input includes 2cc8df2292.
    (self: _prev: {
      neovide = self.master.neovide;
    })

    # nixpkgs 44cddf92c432 "rustPlatform.cargoSetupHook: Force system libs"
    # exports ZSTD_SYS_USE_PKG_CONFIG=1, RUSTONIG_SYSTEM_LIBONIG=1,
    # AWS_LC_SYS_USE_SYSTEM=1 (and friends), so the `zstd-sys`, `onig_sys`
    # and `aws-lc-sys` build scripts demand the system library through
    # pkg-config instead of compiling their vendored copy. Five Rust
    # packages in the pinned nixos-unstable lack the matching
    # `buildInputs` and their builds panic in the crate build script:
    #   codex 0.167.0  -> zstd-sys (and later aws-lc-sys/onig_sys/rusqlite)
    #   wprs           -> zstd-sys
    #   ccusage        -> zstd-sys
    #   aichat         -> onig_sys
    #   nono           -> aws-lc-sys
    # Adding the libs to `buildInputs` puts their `.pc` files on
    # PKG_CONFIG_PATH; every one of the affected packages already ships
    # `pkg-config` in `nativeBuildInputs`. All other failures in the affected
    # builds (fish-completions, agent-bubblewrap wrappers, home-manager,
    # toplevel) are cascading "N dependencies failed" errors.
    #
    # Upstream status:
    # - codex fixed by 16b463698a5b "codex: fix build with sys libs"
    #   (adds aws-lc, oniguruma, sqlite, zstd — mirrored here 1:1)
    # - aichat fixed on master (oniguruma in buildInputs)
    # - wprs, ccusage, nono: no fix on any branch yet (2026-11-08)
    # - voxtype-onnx (vendoring onig_sys and libsqlite3-sys): no fix on any
    #   branch yet (2026-11-09)
    #
    # Appending is idempotent once a package carries the fix upstream, but
    # leaves duplicate buildInputs — drop each entry when the pinned
    # nixos-unstable includes it.
    #
    # See doc/TODOs/drop-rust-force-system-libs-overrides.md.
    (_final: prev: {
      codex = prev.codex.overrideAttrs (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [
          prev.aws-lc
          prev.oniguruma
          prev.sqlite
          prev.zstd
        ];
      });
      wprs = prev.wprs.overrideAttrs (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [ prev.zstd ];
      });
      ccusage = prev.ccusage.overrideAttrs (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [ prev.zstd ];
      });
      aichat = prev.aichat.overrideAttrs (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [ prev.oniguruma ];
      });
      nono = prev.nono.overrideAttrs (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [ prev.aws-lc ];
      });
      # voxtype's upstream package.nix predates the cargoSetupHook change;
      # its Cargo.lock vendors onig_sys and libsqlite3-sys, both forced to
      # the system lib by the hook. `openssl-sys` is covered by the `openssl`
      # already in buildInputs. Attribute is `voxtype-onnx`, not `voxtype`
      # (see the osd-gtk4 override above).
      voxtype-onnx = prev.voxtype-onnx.overrideAttrs (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [
          prev.oniguruma
          prev.sqlite
        ];
      });
    })

    # waybar 0.16.0 (nixpkgs bump landed with the 2026-11-12 system switch)
    # re-based the custom/* module from ALabel to AIconLabel and gave
    # niri/workspaces a per-workspace Workspace widget. Both wrap the
    # previously-bare Gtk::Label in a Gtk::Box packed without expand/fill,
    # so on a vertical bar the text hugs the module's start edge and CSS
    # backgrounds (the blinking error state of custom/mbsync-get-status)
    # cover only the glyph instead of the full module width. GTK3 CSS
    # cannot restore the 0.15.0 layout, so patch the packing to fill the
    # label along the box's main axis.
    #
    # See doc/TODOs/drop-waybar-0.16-label-centering-patch.md.
    #
    # TODO: remove once a waybar release fixes these packing regressions
    # and the nixpkgs input carries it.
    (_final: prev: {
      waybar = prev.waybar.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          ./modules/myconfig.desktop.wayland/programs.waybar/patches/fix-0.16-label-centering.patch
        ];
      });
    })
  ];
}

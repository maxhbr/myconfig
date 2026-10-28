# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# ThinkRail (https://github.com/JetBrains/thinkrail) — agentic IDE built
# around the pi coding agent, packaged from the official prebuilt release:
# a Bun `--compile` standalone binary (~130 MB) embedding engine host,
# web UI, skills, and extensions. Building from source is impractical
# (Bun compile toolchain + many vendored native modules).
#
# The binary must NOT be patchelf'd: Bun standalone executables locate
# their embedded payload via their own ELF section table at runtime;
# patchelf shifts sections and Bun falls back to plain-CLI mode
# (`bun --help`). On Linux the binary hardcodes the FHS interpreter
# /lib64/ld-linux-x86-64.so.2, so it ships unmodified and runs inside an
# FHS bubblewrap env; on macOS it is a plain Mach-O executable.
#
# The version and the per-platform fetchurl hashes are not hardcoded here:
# they come from the nvfetcher-pinned entries in `_sources/generated.nix`
# (see `nvfetcher.toml`, regenerated with `nix run nixpkgs#nvfetcher`), so
# the weekly update-sources CI job bumps this package automatically.
{
  lib,
  stdenvNoCC,
  pkgs,
  buildFHSEnv,
  makeWrapper,
  # nvfetcher-pinned ThinkRail release assets; `src` of each entry is a
  # `fetchurl` store path for the prebuilt binary of one platform.
  sources ? pkgs.callPackage ../../_sources/generated.nix { },
}:

let
  inherit (stdenvNoCC) hostPlatform;

  throwSystem = throw "unsupported system: ${hostPlatform.system} (upstream has no ThinkRail build for it)";

  # Only the platforms upstream ships; there is no x86_64-darwin build.
  platformMap = {
    x86_64-linux = "linux-x64";
    aarch64-linux = "linux-arm64";
    aarch64-darwin = "darwin-arm64";
  };
  platform = platformMap.${hostPlatform.system} or throwSystem;

  # nvfetcher tracks the latest release tag (`v0.2.0`, ...); the package
  # version drops the leading `v`.
  release = sources."thinkrail-${platform}";
  version = lib.removePrefix "v" release.version;
  src = release.src;

  # FHS env providing /lib64/ld-linux-x86-64.so.2 for the unmodified binary.
  # `runScript = ""` makes /init `exec "$@"`, so the wrapper decides what to
  # run; buildFHSEnv's bwrap mounts keep HOME, DISPLAY, and PATH working.
  # `version` is required: buildFHSEnv derives `name` from pname+version.
  fhs = buildFHSEnv {
    pname = "thinkrail-fhs";
    version = "0";
    targetPkgs = pkgs: [ pkgs.glibc ];
    runScript = "";
  };
in
stdenvNoCC.mkDerivation {
  pname = "thinkrail";
  inherit version src;

  # src is a single prebuilt file, not an archive.
  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  nativeBuildInputs = lib.optional hostPlatform.isLinux makeWrapper;

  installPhase =
    if hostPlatform.isLinux then
      ''
        runHook preInstall
        mkdir -p $out/bin $out/share/thinkrail
        install -m 0555 $src $out/share/thinkrail/thinkrail
        makeWrapper ${lib.getExe fhs} $out/bin/thinkrail \
          --add-flags "$out/share/thinkrail/thinkrail"
        runHook postInstall
      ''
    else
      ''
        runHook preInstall
        install -Dm 0555 $src $out/bin/thinkrail
        runHook postInstall
      '';

  meta = {
    description = "Agentic IDE built around the pi coding agent (prebuilt upstream binary)";
    longDescription = ''
      ThinkRail is an agentic IDE built around the pi coding agent. This
      package wraps the official prebuilt single-file binary: it boots the
      engine host in-process and serves the bundled web UI at
      http://localhost:24242.
    '';
    homepage = "https://github.com/JetBrains/thinkrail";
    license = lib.licenses.asl20;
    mainProgram = "thinkrail";
    platforms = builtins.attrNames platformMap;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}

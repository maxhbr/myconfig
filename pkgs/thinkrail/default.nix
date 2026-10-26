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
{
  lib,
  stdenvNoCC,
  fetchurl,
  buildFHSEnv,
  makeWrapper,
  version ? "0.2.0",
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

  src = fetchurl {
    url = "https://github.com/JetBrains/thinkrail/releases/download/v${version}/thinkrail-${platform}";
    # sha256 per platform, cross-checked against the release's SHA256SUMS asset.
    sha256 = {
      x86_64-linux = "sha256-VA99RiWEkWExin2wU3GbGbQgHrf86vTdVWykx+CIAQw=";
      aarch64-linux = "sha256-tH8P9mM3RVtUTpYfXL7cTCtaCPQZbmpBvqJO0xWaYVQ=";
      aarch64-darwin = "sha256-8Qf1PaVBZjvaUcbA7UE2yQs3gCYXsl/PSuWinSb1gwg=";
    }.${hostPlatform.system} or throwSystem;
  };

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

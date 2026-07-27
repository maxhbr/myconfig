# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Prometheus exporter for AMD CPU & GPU metrics, based on
# https://github.com/amd/amd_smi_exporter — a small Go binary that
# reads data via the AMD SMI library (amdsmi) using the ROCm
# `goamdsmi` cgo binding and serves them at /metrics.
#
# nixpkgs has no package for this exporter, so we build it inline with
# buildGoModule. Upstream ships no `go.mod`/`go.sum` (the Makefile does
# a `go mod init` + `go get` at build time), so we generate a pinned
# pair (embedded below as `goMod`/`goSum`) and drop them into the
# source tree at build time. They were generated once with the
# resolved `github.com/ROCm/amdsmi` pseudo-version so the build stays
# reproducible without a committed sidecar directory.
#
# The exporter is deprecated upstream in favour of the AMD Device
# Metrics Exporter (https://github.com/rocm/device-metrics-exporter),
# but amd_smi_exporter is tiny and works fine for the Strix Halo iGPU.
#
# All exported metrics are prefixed `amd_` (e.g. `amd_gpu_power`,
# `amd_gpu_current_temperature`, `amd_gpu_use_percent`). They are
# scraped by the local vmagent (job=`amd-smi`) which pushes them to the
# central VictoriaMetrics via remote_write.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.observability;
  clientCfg = cfg.client;
  amdSmiCfg = clientCfg.amdSmiExporter;

  amdsmi = pkgs.rocmPackages.amdsmi;

  # Pinned go.mod / go.sum for the exporter (upstream ships neither).
  # Regenerate by running, in a checkout of amd_smi_exporter/src:
  #   go mod init src
  #   go get github.com/ROCm/amdsmi@amd-staging \
  #          github.com/prometheus/client_golang
  #   go mod tidy
  # then refresh `vendorHash` below.
  goMod = ''
    module src

    go 1.26.5

    require (
    	github.com/ROCm/amdsmi v0.0.0-20251117222445-a044536b8d69
    	github.com/prometheus/client_golang v1.24.1
    )

    require (
    	github.com/beorn7/perks v1.0.1 // indirect
    	github.com/cespare/xxhash/v2 v2.3.0 // indirect
    	github.com/munnerz/goautoneg v0.0.0-20191010083416-a7dc8b61c822 // indirect
    	github.com/prometheus/client_model v0.6.2 // indirect
    	github.com/prometheus/common v0.70.1 // indirect
    	github.com/prometheus/procfs v0.21.1 // indirect
    	golang.org/x/sys v0.47.0 // indirect
    	google.golang.org/protobuf v1.36.11 // indirect
    )
  '';

  goSum = ''
    github.com/ROCm/amdsmi v0.0.0-20251117222445-a044536b8d69 h1:0Sl/RcyHZvSstVPIbdF0D/sdj8ZJd+xBxkCy5M8/aCI=
    github.com/ROCm/amdsmi v0.0.0-20251117222445-a044536b8d69/go.mod h1:c2lzyLAghhTO+y/c3JjKl59JHJliIHwNZOroUfmBQxc=
    github.com/beorn7/perks v1.0.1 h1:VlbKKnNfV8bJzeqoa4cOKqO6bYr3WgKZxO8Z16+hsOM=
    github.com/beorn7/perks v1.0.1/go.mod h1:G2ZrVWU2WbWT9wwq4/hrbKbnv/1ERSJQ0ibhJ6rlkpw=
    github.com/cespare/xxhash/v2 v2.3.0 h1:UL815xU9SqsFlibzuggzjXhog7bL6oX9BbNZnL2UFvs=
    github.com/cespare/xxhash/v2 v2.3.0/go.mod h1:VGX0DQ3Q6kWi7AoAeZDth3/j3BFtOZR5XLFGgcrjCOs=
    github.com/davecgh/go-spew v1.1.1 h1:vj9j/u1bqnvCEfJOwUhtlOARqs3+rkHYY13jYWTU97c=
    github.com/davecgh/go-spew v1.1.1/go.mod h1:J7Y8YcW2NihsgmVo/mv3lAwl/skON4iLHjSsI+c5H38=
    github.com/google/go-cmp v0.7.0 h1:wk8382ETsv4JYUZwIsn6YpYiWiBsYLSJiTsyBybVuN8=
    github.com/google/go-cmp v0.7.0/go.mod h1:pXiqmnSA92OHEEa9HXL2W4E7lf9JzCmGVUdgjX3N/iU=
    github.com/klauspost/compress v1.19.1 h1:VsB4HPswih7mmZ8WleSFQ75c/Ui1M4trX5oAsJnhSlk=
    github.com/klauspost/compress v1.19.1/go.mod h1:cwPg85FWrGar70rWktvGQj8/hthj3wpl0PGDogxkrSQ=
    github.com/kylelemons/godebug v1.1.0 h1:RPNrshWIDI6G2gRW9EHilWtl7Z6Sb1BR0xunSBf0SNc=
    github.com/kylelemons/godebug v1.1.0/go.mod h1:9/0rRGxNHcop5bhtWyNeEfOS8JIWk580+fNqagV/RAw=
    github.com/munnerz/goautoneg v0.0.0-20191010083416-a7dc8b61c822 h1:C3w9PqII01/Oq1c1nUAm88MOHcQC9l5mIlSMApZMrHA=
    github.com/munnerz/goautoneg v0.0.0-20191010083416-a7dc8b61c822/go.mod h1:+n7T8mK8HuQTcFwEeznm/DIxMOiR9yIdICNftLE1DvQ=
    github.com/pmezard/go-difflib v1.0.0 h1:4DBwDE0NGyQoBHbLQYPwSUPoCMWR5BEzIk/f1lZbAQM=
    github.com/pmezard/go-difflib v1.0.0/go.mod h1:iKH77koFhYxTK1pcRnkKkqfTogsbg7gZNVY4sRDYZ/4=
    github.com/prometheus/client_golang v1.24.1 h1:JnJkREXzWxUdCuPFpIWZiPispT9xVV59uiuyR2bPlnU=
    github.com/prometheus/client_golang v1.24.1/go.mod h1:F+oSRECHg4sse5ucfYpYDeIv/hu68Zo0uoHKetWnzcE=
    github.com/prometheus/client_model v0.6.2 h1:oBsgwpGs7iVziMvrGhE53c/GrLUsZdHnqNwqPLxwZyk=
    github.com/prometheus/client_model v0.6.2/go.mod h1:y3m2F6Gdpfy6Ut/GBsUqTWZqCUvMVzSfMLjcu6wAwpE=
    github.com/prometheus/common v0.70.1 h1:1HvjP4D5oL3t8RsPlwxA9onvvStjtIHYE5XuuwOi/PY=
    github.com/prometheus/common v0.70.1/go.mod h1:VdFUQDMZK3VLkurFUVhia6uys/0suUp86TJz5qbJRhc=
    github.com/prometheus/procfs v0.21.1 h1:GljZCt+zSTS+NZq88cyQ1LjZ+RCHp3uVuabBWA5+OJI=
    github.com/prometheus/procfs v0.21.1/go.mod h1:aB55Cww9pdSJVHk0hUf0inxWyyjPogFIjmHKYgMKmtY=
    github.com/stretchr/testify v1.11.1 h1:7s2iGBzp5EwR7/aIZr8ao5+dra3wiQyKjjFuvgVKu7U=
    github.com/stretchr/testify v1.11.1/go.mod h1:wZwfW3scLgRK+23gO65QZefKpKQRnfz6sD981Nm4B6U=
    go.uber.org/goleak v1.3.0 h1:2K3zAYmnTNqV73imy9J1T3WC+gmCePx2hEGkimedGto=
    go.uber.org/goleak v1.3.0/go.mod h1:CoHD4mav9JJNrW/WLlf7HGZPjdw8EucARQHekz1X6bE=
    go.yaml.in/yaml/v2 v2.4.4 h1:tuyd0P+2Ont/d6e2rl3be67goVK4R6deVxCUX5vyPaQ=
    go.yaml.in/yaml/v2 v2.4.4/go.mod h1:gMZqIpDtDqOfM0uNfy0SkpRhvUryYH0Z6wdMYcacYXQ=
    golang.org/x/sys v0.47.0 h1:o7XGOvZQCADBQQ4Y7VNq2dRWQR7JmOUW8Kxx4ZsNgWs=
    golang.org/x/sys v0.47.0/go.mod h1:4GL1E5IUh+htKOUEOaiffhrAeqysfVGipDYzABqnCmw=
    google.golang.org/protobuf v1.36.11 h1:fV6ZwhNocDyBLK0dj+fg8ektcVegBBuEolpbTQyBNVE=
    google.golang.org/protobuf v1.36.11/go.mod h1:HTf+CrKn2C3g5S8VImy6tdcUvCska2kB7j23XfzDpco=
    gopkg.in/yaml.v3 v3.0.1 h1:fxVm/GzAzEWqLHuvctI91KS9hhNmmWOoWu0XTYJS7CA=
    gopkg.in/yaml.v3 v3.0.1/go.mod h1:K4uyk7z7BCEPqu6E+C64Yfv1cQ7kz7rIZviUmN+EgEM=
  '';

  amd-smi-exporter = pkgs.buildGoModule rec {
    pname = "amd_smi_exporter";
    version = "unstable-2025-05-20";

    src = pkgs.fetchFromGitHub {
      owner = "amd";
      repo = "amd_smi_exporter";
      rev = "ad2e1b1d732e33bc5b459651550051eb4b685c57";
      hash = "sha256-OjQhpu+VK9CAoJvc094nx810HTONujdcgq65ij96bTg=";
    };

    # The Go sources live in `src/` and declare `module src`, so build
    # from there and resolve the main package as ".".
    sourceRoot = "source/src";
    subPackages = [ "." ];

    postPatch = ''
      # Upstream ships no go.mod/go.sum — drop in our pinned pair.
      cp ${pkgs.writeText "amd-smi-exporter-go.mod" goMod} go.mod
      cp ${pkgs.writeText "amd-smi-exporter-go.sum" goSum} go.sum

      # The exporter hard-codes `:2021` (bind on all interfaces). Bind
      # to loopback only — the local vmagent is the sole consumer.
      substituteInPlace main.go \
        --replace-fail 'const addr = ":2021"' \
                       'const addr = "127.0.0.1:${toString amdSmiCfg.port}"'
    '';

    preBuild = ''
      # The `github.com/ROCm/amdsmi` cgo binding includes the C++-only
      # <cstdint> header, which fails when cgo compiles the preamble as
      # C. Swap it for the C equivalent — the shim headers only use
      # plain C types (stdint.h / stdbool.h).
      substituteInPlace vendor/github.com/ROCm/amdsmi/goamdsmi.go \
        --replace-fail '#include <cstdint>' '#include <stdint.h>'
    '';

    # Refresh with `vendorHash = lib.fakeHash` + copy the "got:" line if
    # the pinned go.sum ever changes.
    vendorHash = "sha256-2ARVjHkN1KAJgSwsYvJblMcubPuJvo+IbLOj0kow8jE=";

    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = [ amdsmi ];

    env = {
      CGO_CFLAGS = "-I${amdsmi}/include";
      # libgoamdsmi_shim64 has undefined references that are resolved by
      # libamd_smi; link both and let the loader find them via RPATH.
      CGO_LDFLAGS = lib.concatStringsSep " " [
        "-L${amdsmi}/lib"
        "-lgoamdsmi_shim64"
        "-lamd_smi"
        "-Wl,--unresolved-symbols=ignore-in-object-files"
        "-Wl,--allow-shlib-undefined"
        "-Wl,-rpath,${amdsmi}/lib"
      ];
    };

    # No usable tests (they poll real hardware).
    doCheck = false;

    # The main package is `src`, so the binary is named `src` — rename
    # it to the canonical name.
    postInstall = ''
      mv $out/bin/src $out/bin/amd_smi_exporter
    '';

    meta = with lib; {
      description = "Prometheus exporter for AMD CPU & GPU metrics via the AMD SMI library";
      homepage = "https://github.com/amd/amd_smi_exporter";
      license = licenses.mit;
      platforms = [ "x86_64-linux" ];
      mainProgram = "amd_smi_exporter";
    };
  };
in
{
  options.myconfig.observability.client.amdSmiExporter = with lib; {
    enable = mkEnableOption ''
      amd_smi_exporter for AMD CPU & GPU metrics (ROCm / amdsmi). Only
      makes sense on hosts with a ROCm-capable AMD GPU — the exporter
      needs the amdsmi library and the kfd/dri device nodes.
    '';

    port = mkOption {
      type = types.port;
      default = 2021;
      description = ''
        Port the amd_smi_exporter listens on (loopback only). Baked
        into the binary at build time (upstream hard-codes it), so
        changing this rebuilds the exporter.
      '';
    };
  };

  config = lib.mkIf (clientCfg.enable && amdSmiCfg.enable) {
    systemd.services.prometheus-amd-smi-exporter = {
      description = "Prometheus amd_smi_exporter (AMD CPU/GPU metrics)";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      # `rocm-smi` is invoked (best-effort) to label GPUs with their
      # product/card series name; without it the exporter still works
      # but omits the friendly name.
      path = [ pkgs.rocmPackages.rocm-smi ];

      serviceConfig = {
        ExecStart = lib.getExe amd-smi-exporter;
        Restart = "on-failure";
        RestartSec = "10s";

        DynamicUser = true;
        # Needs access to the AMD GPU/KFD device nodes.
        SupplementaryGroups = [
          "video"
          "render"
        ];
        DeviceAllow = [
          "/dev/kfd rw"
          "/dev/dri rw"
        ];

        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
          "AF_NETLINK"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
      };
    };

    services.vmagent = {
      prometheusConfig = {
        scrape_configs = [
          {
            job_name = "amd-smi";
            static_configs = [
              { targets = [ "127.0.0.1:${toString amdSmiCfg.port}" ]; }
            ];
          }
        ];
      };
    };
  };
}

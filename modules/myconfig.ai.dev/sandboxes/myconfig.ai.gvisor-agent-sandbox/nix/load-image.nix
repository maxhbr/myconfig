{
  lib,
  runCommand,
  writeShellApplication,
  coreutils,
  gnutar,
  gzip,
  jq,
  podman,
  agent-gvisor-image,
}:

let
  ref = "${agent-gvisor-image.imageName}:${agent-gvisor-image.imageTag}";

  # The identity of an OCI image is the digest of its config blob: Podman
  # reports it as the image ID, and in a docker-archive it is the name of the
  # config file recorded in `manifest.json` (`<sha256hex>.json`). Comparing
  # that digest with the loaded image's ID answers "is the artifact in the
  # Podman store the one this Nix build produced?" — a tag alone cannot,
  # because `localhost/agent-dev:latest` is reused by every rebuild.
  #
  # Extracting it needs the whole (compressed) tarball to be read, so do it
  # once at build time and bake the resulting one-line file into the script
  # instead of paying for it on every invocation.
  imageIdFile =
    runCommand "agent-gvisor-image-id"
      {
        nativeBuildInputs = [
          gnutar
          gzip
          jq
        ];
      }
      ''
        tar --extract --to-stdout --file ${agent-gvisor-image} manifest.json \
          | jq -r '.[0].Config | rtrimstr(".json") | ltrimstr("sha256:")' > $out
      '';
in
writeShellApplication {
  name = "agent-gvisor-load-image";

  runtimeInputs = [
    coreutils
    podman
  ];

  text = ''
    image=${agent-gvisor-image}
    ref=${ref}
    expected=$(cat ${imageIdFile})

    usage() {
      cat <<EOF
    Usage: agent-gvisor-load-image [--force|--test|--help]

    Loads the Nix-built agent sandbox image into the caller's Podman store.
    Without options it loads the image when it is missing or when the loaded
    one is a different build than the current artifact.

      --force   reload unconditionally
      --test    do not load anything; report the state and exit 0 only if the
                current artifact is already the loaded one (1 otherwise)
      --help    show this text
    EOF
    }

    # Image ID of $ref in the local store, empty if it is not present.
    loaded_id() {
      local id
      id=$(podman image inspect --format '{{.Id}}' "$ref" 2>/dev/null) || return 0
      printf '%s\n' "''${id#sha256:}"
    }

    # state: absent | stale | current
    report() {
      local loaded=$1 state=$2 shown
      shown=''${loaded:+sha256:$loaded}
      printf 'image:    %s\n' "$image"
      printf 'ref:      %s\n' "$ref"
      printf 'expected: sha256:%s\n' "$expected"
      printf 'loaded:   %s\n' "''${shown:--}"
      printf 'state:    %s\n' "$state"
    }

    classify() {
      if [ -z "$1" ]; then
        printf 'absent\n'
      elif [ "$1" = "$expected" ]; then
        printf 'current\n'
      else
        printf 'stale\n'
      fi
    }

    mode=''${1:-}
    case "$mode" in
      --help | -h)
        usage
        exit 0
        ;;
      --test)
        loaded=$(loaded_id)
        state=$(classify "$loaded")
        report "$loaded" "$state"
        if [ "$state" = current ]; then exit 0; else exit 1; fi
        ;;
      --force | "") ;;
      *)
        printf 'unknown option: %s\n' "$mode" >&2
        usage >&2
        exit 2
        ;;
    esac

    loaded=$(loaded_id)
    state=$(classify "$loaded")

    if [ "$mode" = --force ] || [ "$state" != current ]; then
      case "$state" in
        absent) printf 'loading %s as %s\n' "$image" "$ref" >&2 ;;
        stale) printf 'replacing %s (sha256:%s) with %s\n' "$ref" "$loaded" "$image" >&2 ;;
        current) printf 'reloading %s as %s (--force)\n' "$image" "$ref" >&2 ;;
      esac
      podman load --input "$image"
    else
      printf '%s is already the current build; pass --force to reload\n' "$ref" >&2
    fi

    loaded=$(loaded_id)
    state=$(classify "$loaded")
    report "$loaded" "$state" >&2
    # A load that does not end in `current` means the store holds something
    # other than this build under the tag; do not report success for that.
    [ "$state" = current ]
  '';

  meta = {
    description = "Load the Nix-built agent sandbox image into Podman";
    mainProgram = "agent-gvisor-load-image";
    platforms = lib.platforms.linux;
  };
}

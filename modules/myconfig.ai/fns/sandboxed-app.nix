{
  lib,
  pkgs,
}:

{
  name,
  pkg,
  extraRuntimeInputs ? [ ],
  readOnlyConfigDirs ? [ ],
  writableDirs ? [
    ".local/share"
    ".cache"
    ".local/state"
  ],
  extraBwrapArgs ? [ ],
  envVars ? { },
  shareNet ? true,
  hostname ? "${name}-sandbox",
}:

let
  runtimeInputs = extraRuntimeInputs;

  readOnlyDirsStr = lib.concatMapStringsSep " " (dir: ''"$HOME/${dir}"'') readOnlyConfigDirs;

  roBindArgs = lib.concatMapStringsSep "\n      " (
    dir: ''--ro-bind "$HOME/${dir}" "$HOME/${dir}"''
  ) readOnlyConfigDirs;

  envArgs = lib.concatStringsSep "\n      " (
    lib.mapAttrsToList (k: v: ''--setenv ${k} "${v}"'') envVars
  );
in

pkgs.writeShellApplication {
  name = "${name}-bwrap";
  runtimeInputs = runtimeInputs;
  text = ''
    set -euo pipefail

    PWD_REAL="$(pwd -P)"

    XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"
    XDG_DATA_HOME="''${XDG_DATA_HOME:-$HOME/.local/share}"
    XDG_CACHE_HOME="''${XDG_CACHE_HOME:-$HOME/.cache}"
    XDG_STATE_HOME="''${XDG_STATE_HOME:-$HOME/.local/state}"

    ${lib.optionalString (readOnlyConfigDirs != [ ]) "mkdir -p ${readOnlyDirsStr}"}
    mkdir -p "$XDG_DATA_HOME" "$XDG_CACHE_HOME" "$XDG_STATE_HOME"

    args=(
      --unshare-all
      ${lib.optionalString shareNet "--share-net"}
      --die-with-parent
      --new-session
      --hostname ${hostname}
      --ro-bind /proc /proc
      --ro-bind /run /run
      --tmpfs /dev
      --dev-bind /dev/null /dev/null
      --dev-bind /dev/zero /dev/zero
      --dev-bind /dev/random /dev/random
      --dev-bind /dev/urandom /dev/urandom
      --tmpfs /tmp

      --tmpfs "$HOME"
      --dir "$HOME/.config"
      --dir "$HOME/.local"
      --dir "$HOME/.cache"

      ${roBindArgs}
      --bind "$XDG_DATA_HOME"  "$XDG_DATA_HOME"
      --bind "$XDG_CACHE_HOME" "$XDG_CACHE_HOME"
      --bind "$XDG_STATE_HOME" "$XDG_STATE_HOME"

      --bind "$PWD_REAL" "$PWD_REAL"
      --chdir "$PWD_REAL"

      --setenv HOME "$HOME"
      --setenv XDG_CONFIG_HOME "$XDG_CONFIG_HOME"
      --setenv XDG_DATA_HOME "$XDG_DATA_HOME"
      --setenv XDG_CACHE_HOME "$XDG_CACHE_HOME"
      --setenv XDG_STATE_HOME "$XDG_STATE_HOME"
      ${envArgs}
    )

    for p in /usr /bin /sbin /lib /lib64; do
      if [ -e "$p" ]; then
        args+=( --ro-bind "$p" "$p" )
      fi
    done

    if [ -e /nix/store ]; then
      args+=( --ro-bind /nix /nix )
    fi

    for p in /etc/resolv.conf /etc/hosts /etc/ssl /etc/ca-certificates /etc/static; do
      if [ -e "$p" ]; then
        args+=( --ro-bind "$p" "$p" )
      fi
    done

    # Worktree support: when launched from a git worktree (e.g. via a workmux
    # `*-worktree` wrapper) the current directory's `.git` is a file pointing
    # into the linked main repository's `.git/worktrees/<name>/`. Bind the main
    # repo read-only and remount its shared git dir read-write so git works
    # inside the sandbox. The launcher sets these env vars; they are ignored
    # for plain (non-worktree) invocations.
    if [ -n "''${WORKTREE_MAIN_REPO:-}" ] && [ -e "''${WORKTREE_MAIN_REPO}" ]; then
      args+=( --ro-bind "$WORKTREE_MAIN_REPO" "$WORKTREE_MAIN_REPO" )
    fi
    if [ -n "''${WORKTREE_GIT_DIR:-}" ] && [ -e "''${WORKTREE_GIT_DIR}" ]; then
      args+=( --bind "$WORKTREE_GIT_DIR" "$WORKTREE_GIT_DIR" )
    fi

    exec ${lib.getExe pkgs.bubblewrap} "''${args[@]}" -- ${lib.getExe pkg} "$@"
  '';

  meta = with lib; {
    description = "Sandboxed ${name} wrapper using bubblewrap";
    maintainers = [ ];
    platforms = platforms.linux;
  };
}

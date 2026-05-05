{
  combinators,
  helpers,
  pkgs,
  ...
}:
let
  inherit (combinators) add-runtime compose include-once;
in
{
  sig = "String -> Permission";
  doc = ''
    This is an internal (for now) combinator to bind a path at runtime in a
    way.

    This is marked as internal which means it isn't publicly documented on
    the website since this will likely change in the future. Feel free to
    use, but a future update may break this combinator.
  '';
  internal = true;
  impl =
    let
      bindRuntimeHelperFunction = include-once "bindRuntimeHelperFunction" (add-runtime ''
        function bindNixStoreClosure {
          local NIX_STORE_PATH
          NIX_STORE_PATH="$1"
          while read -r P; do
            RUNTIME_ARGS+=(--ro-bind "$P" "$P")
          done <<< "$(${pkgs.nix}/bin/nix-store --query --requisites "$NIX_STORE_PATH")"
        }

        function bindRuntime {
          local MAX_DEPTH
          local PATH_TO_BIND
          MAX_DEPTH="$1"
          PATH_TO_BIND="$2"
          if ((MAX_DEPTH <= 0)); then
            echo "jail.nix: bindRuntime hit max depth" >&2
            exit 1
          fi
          if ! [ -e "$PATH_TO_BIND" ]; then
            return
          fi
          case $(stat -c '%F' "$PATH_TO_BIND") in
            'regular file'|socket)
              RUNTIME_ARGS+=(--ro-bind "$PATH_TO_BIND" "$PATH_TO_BIND")
            ;;
            directory)
              for DIR_ENTRY in "$PATH_TO_BIND"/*; do
                [ -e "$DIR_ENTRY" ] || continue
                bindRuntime "$((MAX_DEPTH - 1))" "$DIR_ENTRY"
              done
            ;;
            'symbolic link')
              LINK_PATH=$(realpath "$PATH_TO_BIND")
              RUNTIME_ARGS+=(--symlink "$LINK_PATH" "$PATH_TO_BIND")
              if [[ "$LINK_PATH" == /nix/store/* ]]; then
                bindNixStoreClosure "$LINK_PATH"
              fi
            ;;
          esac
        }
      '');
    in
    path:
    compose [
      bindRuntimeHelperFunction
      (add-runtime "bindRuntime 5 ${helpers.escape path}")
    ];
}

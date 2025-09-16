{ self, ... }@inputs:
system:
let
  pkgs = inputs.nixpkgs.legacyPackages."${system}";
  fmt =
    with pkgs;
    writeShellScriptBin "fmt" ''
      set -euo pipefail

      format() {
        local file="$1"
        shift
        local nixfmt_args="$@"
        time find "$file" \
                -type f \
                -iname '*.nix' \
                -not -iname 'empty_nixos_config.nix' \
                -print0 | xargs -0 -n1 ${nixfmt-rfc-style}/bin/nixfmt $nixfmt_args
      }

      check() {
        local file="$1"
        format "$file" "--check"
      }

      format_and_handle_git() {
          local file="$1"
          local REASON_TO_NOT_DO_COMMIT=""

          if ! ${git}/bin/git rev-parse --git-dir > /dev/null 2>&1; then
              echo "Not in a git repository"
              exit 1
          else
              # Check if the working directory is clean
              if ! ${git}/bin/git diff-index --quiet HEAD --; then
                  echo "Working directory is not clean"
                  REASON_TO_NOT_DO_COMMIT="working directory is not clean"

                  read -p "Do you want to continue? It will not commit, just format (y/n) " -n 1 -r
                  echo
                  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                      echo "Aborting"
                      exit 0
                  fi
              fi
          fi

          format "$file"

          if [ -z "$REASON_TO_NOT_DO_COMMIT" ]; then
              # Check if any files were modified
              if ! ${git}/bin/git diff-index --quiet HEAD --; then
                  echo "Files were modified by nixfmt, committing changes..."
                  ${git}/bin/git add -A
                  ${git}/bin/git commit -m "nixfmtall"
                  echo "Changes committed successfully"
              else
                  echo "No files were modified by nixfmt"
              fi
          else
              echo "Skipping commit because $REASON_TO_NOT_DO_COMMIT"
          fi
      }

      ##############################################################################
      ## check arguments
      ##############################################################################

      action="format"

      if [[ $# -gt 0 && "$1" == "--check" ]]; then
          shift
          action="check"
      fi


      if [[ $# -gt 0 ]]; then
          arg="$1"
          if [[ -d "$arg" ]]; then
              cd "$arg"
              file="."
          elif [[ -f "$arg" ]]; then
              cd "$(dirname "$arg")"
              file="$(basename "$arg")"
          else
              echo "Invalid argument: $arg"
              exit 1
          fi
      else
          file="."
      fi

      file="$(readlink -f "$file")"
      echo "run on: $file" >&2

      ##############################################################################
      ## running
      ##############################################################################

      if [[ "$action" == "format" ]]; then
          format_and_handle_git "$file"
      elif [[ "$action" == "check" ]]; then
          check "$file"
      fi

    '';
in
{
  type = "app";
  program = "${fmt}/bin/fmt";
}

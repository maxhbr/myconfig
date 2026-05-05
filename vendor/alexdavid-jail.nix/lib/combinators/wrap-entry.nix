{ lib, pkgs, ... }:
{
  sig = "(String -> String) -> Permission";
  doc = ''
    Wraps the binary to be jailed in a bash script that will be the new
    entrypoint to the jail.

    This similar in spirit to the [add-runtime combinator](#add-runtime),
    except that this runs *inside* the jail, while `add-runtime` runs before
    the jail starts.

    Example:
    ```nix
    wrap-entry (entry: ${"''"}
      echo 'Inside the jail!'
      ''${entry}
      echo 'Cleaning up...'
    ${"''"})
    ```
  '';
  impl =
    getWrapper: state:
    state
    // {
      entry = lib.getExe (
        pkgs.writeShellApplication {
          name = "${state.name}-jail-wrapper";
          text = getWrapper "${state.entry} \"$@\"";
        }
      );
    };
}

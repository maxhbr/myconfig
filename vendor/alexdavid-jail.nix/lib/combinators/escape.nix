{ helpers, ... }:
{
  sig = "String -> String";
  doc = ''
    Shell escapes the passed string.

    Use [noescape](#noescape) to prevent escaping.

    `escape` and `noescape` don't return `Permission`s, but they are useful
    helpers to expose when defining jails and writing custom combinators, so
    they are exposed with the rest of the combinators for convenience.

    Example:
    ```nix
    jail-nix.lib.extend {
      inherit pkgs;
      additionalCombinators = combinators: with combinators; {
        # a combinator that binds the passed path to /foo
        my-combinator = path: unsafe-add-raw-args "--bind ''${escape path} /foo"
      };
    }
    ```
  '';
  impl = helpers.escape;
}

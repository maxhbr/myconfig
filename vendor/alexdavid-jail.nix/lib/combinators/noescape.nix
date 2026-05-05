{ helpers, ... }:
{
  sig = "String -> NoEscapedString";
  doc = ''
    Prevent the passed string from being automatically shell escaped.

    `escape` and `noescape` don't return `Permission`s, but they are useful
    helpers to expose when defining jails and writing custom combinators, so
    they are exposed with the rest of the combinators for convenience.

    It is the caller's responsibility to ensure anything passed to this is
    correctly escaped.

    ```nix
    # Probably doesn't do what you intended since "~/foo" is shell escaped:
    (readonly "~/foo")

    # This properly makes $HOME/foo readonly in the jail:
    (readonly (noescape "~/foo"))

    # Binds the path specified by the runtime $FOO variable as read only.
    #
    # Note that we must properly quote this to ensure bash correctly keeps it
    # as a single argument, even if it contains spaces:
    (readonly (noescape "\"$FOO\""))
    ```
  '';
  impl = helpers.noescape;
}

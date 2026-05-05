{ combinators, helpers, ... }:
let
  inherit (combinators)
    add-runtime
    compose
    noescape
    rw-bind
    ;
in
{
  sig = "String -> Permission";
  doc = ''
    Persists the home directory across all jails with the specified name.

    This is useful for a lot of software that may want to write arbitrary
    things into your home directory and expect to read them back in a future
    invocation.

    The home directory is persisted in `~/.local/share/jail.nix/home/<name>`.
  '';
  impl =
    name:
    compose [
      (add-runtime "mkdir -p ${helpers.dataDirSubPath "home/${name}"}")
      (rw-bind (noescape (helpers.dataDirSubPath "home/${name}")) (noescape "~"))
    ];
}

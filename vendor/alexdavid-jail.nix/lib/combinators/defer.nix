{ helpers, ... }:
{
  sig = "Permission -> Permission";
  doc = ''
    Runs the passed permission after all other permissions.

    A `Permission` is just a `State -> State` function. For many
    permissions order does not matter since they touch different parts of
    state, or they add bubblewrap args that don't affect each other.

    However, some more complicated permissions may need to operate on the
    final state. By calling `defer`, this permission will be applied after
    all other base and per-jail permissions.

    This is useful when writing your own combinators, specifically ones
    that operate on state modified by other permissions. This is especially
    useful if your combinator is called in `basePermissions` since those
    are applied before any of the per-jail permissions are.

    Example:
    ```nix
    let
      print-hostname = state:
        jail.combinators.add-runtime "echo 'Hostname is: ''${state.hostname}'"
        state;
    in jail "test" pkgs.hello (combinators: with combinators; [
      # This will print 'Hostname is: jail':
      print-hostname

      # This will print 'Hostname is: foo'
      (defer print-hostname)

      (set-hostname "foo")
    ])
    ```
  '';
  impl = helpers.pushState "deferredPermissions";
}

_: {
  sig = "Permission";
  doc = ''
    Resets the jail state back to the initial empty state.

    All permissions before this one (including ones set in
    [`basePermissions`](advanced-configuration.md#basepermissions)) are
    removed.

    This is useful if you have some permissions set in
    [`basePermissions`](advanced-configuration.md#basepermissions) that you
    want for most of your jails, but want to remove them in a one-off jail.

    Example:
    ```nix
      jail "some-package" some-package (combinators: with combinators; [
        # First, use reset to remove the base combinators:
        reset

        # Then, re-apply the base combinators you want:
        base
        bind-nix-store-runtime-closure
        fake-passwd
      ])
    ```
  '';
  impl = state: state.initialState;
}

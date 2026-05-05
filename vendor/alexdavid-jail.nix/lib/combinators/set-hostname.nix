{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Sets the hostname to use for the `network` combinator.

    Must be specified before `network`.

    Example:
    ```nix
    [
      (set-hostname "foo")
      network
    ]
    ```
  '';
  impl = hostname: state: state // { inherit hostname; };
}

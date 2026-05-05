{ lib, ... }:
{
  sig = "[Permission] -> Permission";
  doc = ''
    Allows combinator composition.

    `compose [ a b c ]` combines `a`, `b`, and `c`.

    This is useful when writing your own combinators, for example when using
    `jail-nix.lib.extend`:

    ```nix
    jail-nix.lib.extend {
      inherit pkgs;
      additionalCombinators = combinators: with combinators; {
        mycombinator = compose [
          (readonly "/foo")
          (readonly "/bar")
          gpu
        ];
      };
    }
    ```
  '';
  impl = lib.flip lib.pipe;
}

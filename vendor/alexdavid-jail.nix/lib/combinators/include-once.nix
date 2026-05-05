{ helpers, lib, ... }:
{
  sig = "String -> Permission -> Permission";
  doc = ''
    Only run the passed permission if include-once hasn't been previously
    called with the specified key.

    This is useful when writing your own combinators.

    ```nix
    let
      jail = jail-nix.lib.extend {
        inherit pkgs;
        additionalCombinators = combinators: with combinators; {
          # foo isn't `include-once` so each call to it adds a new echo
          foo = add-runtime "echo foo";
          # bar will only be included once, no matter how many times it is called
          bar = include-once "bar" (add-runtime "echo bar");
        };
      };
    in
      # Prints:
      # foo
      # foo
      # foo
      # bar
      # Hello, world!
      jail "test" pkgs.hello (c: with c; [
        foo
        foo
        foo
        bar
        bar
        bar
      ])
    ```
  '';
  impl =
    key: combinator: state:
    if lib.elem key state.includedOnce then
      state
    else
      combinator (helpers.pushState "includedOnce" key state);
}

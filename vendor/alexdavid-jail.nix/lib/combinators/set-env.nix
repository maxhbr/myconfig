{ helpers, ... }:
{
  sig = "String -> String -> Permission";
  doc = ''
    Sets the specified environment variable in the jail.

    This will throw if the variable name is not a valid posix variable name.
  '';
  impl =
    name: value: state:
    state
    // {
      env = state.env // {
        ${name} = helpers.escape value;
      };
    };
}

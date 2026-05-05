{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Prepends the passed string to `$PATH`.
  '';
  impl =
    path: state:
    state
    // {
      env = state.env // {
        PATH = if state.env ? PATH && state.env.PATH != "" then "${path}:${state.env.PATH}" else path;
      };
    };
}

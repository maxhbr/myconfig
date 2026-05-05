{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Adds the raw string passed into it into the call to bubblewrap.

    Nothing is escaped, it is the caller's responsibility to ensure
    everything is properly escaped.
  '';
  impl = args: state: state // { cmd = "${state.cmd} ${args}"; };
}

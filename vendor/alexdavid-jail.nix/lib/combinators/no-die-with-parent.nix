{ ... }:
{
  sig = "Permission";
  doc = ''
    Disables `--die-with-parent`

    By default, jail-nix includes the `--die-with-parent` bwrap flag which
    kills all processes within this jail when the parent process dies. Use this
    combinator if you don't want this behavior.

    This may be useful if the software you are jailing forks a daemon and then
    exits.
  '';
  impl = state: state // { dieWithParent = false; };
}

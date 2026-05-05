{ ... }:
{
  sig = "Permission";
  doc = ''
    Disables `--new-session`

    By default, jail-nix includes the `--new-session` bwrap flag. Doing this
    prevents a jailed application from being able to feed keyboard input to
    the terminal, however this may break some TUI applications.

    See BWRAP(1) for more information and security implications.
  '';
  impl = state: state // { newSession = false; };
}

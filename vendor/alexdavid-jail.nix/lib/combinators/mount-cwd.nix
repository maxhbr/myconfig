{ combinators, ... }:
let
  inherit (combinators)
    include-once
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  doc = ''
    Bind mounts the runtime working directory as read-write.
  '';
  impl = include-once "mount-cwd" (unsafe-add-raw-args "--bind \"$PWD\" \"$PWD\"");
}

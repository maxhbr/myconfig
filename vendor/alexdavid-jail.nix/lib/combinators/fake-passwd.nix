{
  combinators,
  helpers,
  pkgs,
  ...
}:
let
  inherit (combinators)
    add-runtime
    compose
    noescape
    ro-bind
    ;
in
{
  sig = "Permission";
  includedInBasePermissions = true;
  doc = ''
    Generates and mounts  fake `/etc/passwd` and `/etc/group` files in the jail.

    The fake `/etc/passwd` and `/etc/group` files contains a root user, and
    forward the calling user's user id, username, group id and group name.

    If you do not want to hide the users and groups that exist on your system,
    you may consider just bind mounting `/etc/passwd` and `/etc/group` inside
    the jail instead.
  '';
  impl = compose [
    (add-runtime ''
      if [ ! -e ${helpers.dataDirSubPath "passwd"} ] || [ ! -e ${helpers.dataDirSubPath "group"} ]; then
        NOLOGIN=${pkgs.shadow}/bin/nologin
        mkdir -p ${helpers.dataDir}
        echo "root:x:0:0:System administrator:/root:$NOLOGIN" > ${helpers.dataDirSubPath "passwd"}
        echo "$(id -un):x:$(id -u):$(id -g)::$HOME:$NOLOGIN" >> ${helpers.dataDirSubPath "passwd"}
        echo "root:x:0:" > ${helpers.dataDirSubPath "group"}
        echo "$(id -gn):x:$(id -g):" >> ${helpers.dataDirSubPath "group"}
      fi
    '')
    (ro-bind (noescape (helpers.dataDirSubPath "passwd")) "/etc/passwd")
    (ro-bind (noescape (helpers.dataDirSubPath "group")) "/etc/group")
  ];
}

{ ... }:
{
  sig = "String -> Permission";
  doc = ''
    Removes the call to `--unshare-` for the provided namespace.

    By default, jail-nix unshares all namespaces, this combinator allows you to
    remove the unshare, allowing the jailed app access to the specified host
    namespace.

    For instance, calling `share-ns "pid"` will remove the `--unshare-pid` flag
    from bwrap which will allow this process to share the same pid namespace as
    the host.

    The valid namespaces that can be passed to this combinator are: `"cgroup"`
    `"ipc"` `"net"` `"pid"` `"user"`, and `"uts"`.

    See [wikipedia:Linux
    namespaces](https://en.wikipedia.org/wiki/Linux_namespaces) for more
    information.
  '';
  impl =
    namespace: state:
    state
    // {
      namespaces = state.namespaces // {
        ${namespace} = true;
      };
    };
}

{ combinators, pkgs, ... }:
let
  inherit (combinators)
    add-runtime
    compose
    defer
    include-once
    unsafe-add-raw-args
    ;
in
{
  sig = "Permission";
  includedInBasePermissions = true;
  doc = ''
    Binds all `/nix/store` paths in the runtime closure of the jailed
    application.

    If you don't have any sensitive nix store paths, you may consider just
    bind mounting `/nix/store` inside the jail instead.

    For example, the jail defined by
    ```nix
    # Note that this combinator is included in base permissions, so it does
    # not need to be provided:
    let listNixStore = pkgs.writeShellScriptBin "list-nix-store" "ls -l /nix/store";
    in jail "list-nix-store" listNixStore [];
    ```
    will print something like
    ```
    total 52
    dr-xr-xr-x 3 65534 65534 4096 Jan  1  1970 2dx846w0q80307z72r9jxai4xlj9ghb2-list-nix-store
    dr-xr-xr-x 4 65534 65534 4096 Jan  1  1970 3mi59bgj22xx29dyss7jhmx3sgznd85m-acl-2.3.2
    dr-xr-xr-x 3 65534 65534 4096 Jan  1  1970 6hqzbvz50bm87hcj4qfn51gh7arxj8a6-gcc-14.2.1.20250322-libgcc
    dr-xr-xr-x 4 65534 65534 4096 Jan  1  1970 6nkqdqzpa75514lhglgnjs5k4dklw4sb-libidn2-2.3.8
    dr-xr-xr-x 4 65534 65534 4096 Jan  1  1970 7c0v0kbrrdc2cqgisi78jdqxn73n3401-gcc-14.2.1.20250322-lib
    dr-xr-xr-x 4 65534 65534 4096 Jan  1  1970 87fck6hm17chxjq7badb11mq036zbyv9-coreutils-9.7
    dr-xr-xr-x 5 65534 65534 4096 Jan  1  1970 8syylmkvnn7lg2nar9fddpp5izb4gh56-attr-2.5.2
    dr-xr-xr-x 6 65534 65534 4096 Jan  1  1970 cg9s562sa33k78m63njfn1rw47dp9z0i-glibc-2.40-66
    dr-xr-xr-x 3 65534 65534 4096 Jan  1  1970 nzg6zqsijbv7yc95wlfcdswx6bg69srq-gmp-with-cxx-6.3.0
    -r--r--r-- 1 65534 65534  145 Jan  1  1970 vylji4sibxm3mr3hync4zcmpmgbv09az-list-nix-store-runtime-closure
    dr-xr-xr-x 4 65534 65534 4096 Jan  1  1970 xy4jjgw87sbgwylm5kn047d9gkbhsr9x-bash-5.2p37
    dr-xr-xr-x 3 65534 65534 4096 Jan  1  1970 yypqcvqhnv8y4zpicgxdigp3giq81gzb-libunistring-1.3
    dr-xr-xr-x 3 65534 65534 4096 Jan  1  1970 za53jjhjl1xajv3y1zpjvr9mh4w0c1ay-xgcc-14.2.1.20250322-libgcc
    ```
    rather than your entire nix store.
  '';
  impl = include-once "bind-nix-store-runtime-closure" (
    defer (
      state:
      let
        runtimeClosure = pkgs.writeText "${state.name}-runtime-closure" ''
          ${pkgs.lib.concatStringsSep "\n" state.additionalRuntimeClosures}
          ${state.argv}
          ${state.entry}
        '';
        bindArgs =
          pkgs.runCommand "${state.name}-runtime-closure-bind-args"
            {
              __structuredAttrs = true;
              exportReferencesGraph.runtime = runtimeClosure;
              nativeBuildInputs = [ pkgs.jq ];
            }
            ''
              while read -r DEP; do
                printf '%s\0' --ro-bind "$DEP" "$DEP" >> $out
              done <<< "$(jq -r '.runtime | map(.path) | sort | .[]' "$NIX_ATTRS_JSON_FILE")"
            '';
      in
      compose [
        # Use `--args` and a file descriptor here in case the runtime
        # closure is huge, to avoid running into argv size limitations
        (add-runtime "exec {RUNTIME_CLOSURE_BIND_ARGS_FD}<${bindArgs}")
        (unsafe-add-raw-args "--args \"$RUNTIME_CLOSURE_BIND_ARGS_FD\"")
      ] state
    )
  );
}

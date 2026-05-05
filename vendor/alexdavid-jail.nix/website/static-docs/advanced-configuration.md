# Advanced Configuration

All configuration options on this page can be passed into `jail-nix.lib.extend`.

## additionalCombinators

`additionalCombinators` takes in a list of custom combinators to expose under
`jail.combinators` and in jail definitions. If passed a funciton, jail.nix will
inject the builtin combinators.

For example, this creates a `jail` function that exposes a `my-permission`
combinator:

```nix
jail = jail-nix.lib.extend {
  inherit pkgs;
  additionalCombinators = builtinCombinators: with builtinCombinators; {
    my-permission = compose [
      (readonly "/foo")
      (readonly "/bar")
    ];
  };
};

# Now my-permission is exposed in all the places the builtin combinators are exposed:

jailed-hello = jail "jailed-hello" pkgs.hello (c: with c; [
  my-permission
]);

# Alternatively, using jail.combinators:

jailed-hello = jail "jailed-hello" pkgs.hello [
  jail.combinators.my-permission
];
```

## basePermissions

By default, jail.nix comes with a base set of permissions that all jails
inherit by default. This configuration option allows you to override these.

The goal of the base permissions are to provide a reasonably secure default
with enough permissions to have most software behave correctly.

If you override this, you may want to take a look at the [default included
combinators](combinators.md#default-included-combinators).

For example, here is the default set of base permissions:
```nix
jail = jail-nix.lib.extend {
  inherit pkgs;
  basePermissions = combinators: with combinators; [
    base
    bind-nix-store-runtime-closure
    fake-passwd
  ];
};
```

However if you wanted to bind in your entire nix store rather than using the
bind-nix-store-runtime-closure combinator you could do that like so:

```nix
jail = jail-nix.lib.extend {
  inherit pkgs;
  basePermissions = combinators: with combinators; [
    base
    (readonly "/nix/store")
    fake-passwd
  ];
};
```

## bubblewrapPackage

This configuration option allows you to override what bubblewrap package
jail.nix uses for its jails. The default value is `pkgs.bubblewrap`.

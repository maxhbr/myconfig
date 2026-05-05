# Getting Started

## Installation

### Flakes

To install jail.nix using flakes, add jail.nix to your flake inputs:

```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  outputs = { nixpkgs, jail-nix, ... }: {
    # ...
  };
}
```

### callPackage

To install jail.nix using a different dependency manager like
[niv](https://github.com/nmattia/niv), you can use `pkgs.callPackage`:

```nix
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  jail-nix = pkgs.callPackage sources."jail.nix" {};
  jail = jail-nix.init pkgs;
in
  # ...
```

---

## Initializing

The core functionality of jail.nix is a `jail` function that wraps derivations
in a bubblewrap jail. To set it up, you need to initialize the library with a
version of nixpkgs.

This is done by either calling `jail-nix.lib.init` for a basic setup, or
`jail-nix.lib.extend` for advanced configuration.

### Basic configuration

For a simple working `jail` function, call `jail-nix.lib.init` with an instantiated nixpkgs:
```nix
pkgs = import nixpkgs { system = "x86_64-linux"; };
jail = jail-nix.lib.init pkgs;
```

### Advanced configuration

You can set more options in the initialization with `jail-nix.lib.extend`.

This takes in a single attribute set as an argument. The only required key is
`pkgs` which must be an instantiated nixpkgs.

Example:
```nix
pkgs = import nixpkgs { system = "x86_64-linux"; };
jail = jail-nix.lib.extend {
  inherit pkgs;

  # Other configuration options here...
};
```

For a full list of options see [Advanced Configuration](advanced-configuration.md).

---

## Usage

Once you have an [initialized](#initializing) `jail` function, it takes the
following positional arguments:

* **name**: (String) - the nix store name of wrapper script that is generated.
* **executable to wrap**: (String or Derivation) - An entrypoint to jail, this can
  be a derivation like `pkgs.hello`, or a string like
  `"${pkgs.hello}/bin/hello"`. If a derivation is passed it will call
  `nixpkgs.lib.getExe` on it to get the entrypoint.
* **permissions**: (List of **Permissions**) - The third argument allows you to
  control the permissions of the jail. By default, the wrapped program runs
  with very few permissions and to get it to even work you will need to define
  what you want it to have access to here. For convenience, if this argument is
  a function, jail.combinators will be passed in, and the return value will be
  used.

---

## Overlays

If you prefer, you can jail packages in a nixpkgs overlay using
`jail-nix.lib.mkOverlay`. This takes attribute set with `prev` and `final`
passed in, and `packages` which is a function from combinators to an attribute
set of package name to permissions.

All of the options from [Advanced Configuration](advanced-configuration.md) can
optionally be passed into `mkOverlay` attribute set as well.


Example:

```nix
jailOverlay = final: prev: jail-nix.lib.mkOverlay {
  inherit final prev;
  packages = combinators: with combinators; {
    nodejs = [ network ];
    firefox = [ network gui gpu ];
  };
};
pkgs = import nixpkgs {
  system = "x86_64-linux";
  overlays = [ jailOverlay ];
};
```

This will then override the packages in `pkgs` with jailed versions, but also
expose `pkgs.«name».jailed` and `pkgs.«name».unjailed` on each overlaid
package. The `unjailed` is useful to get access to the original unjailed
package, and `jailed` is nice to use if you want to be explicit in the fact
that you are using the jailed package.

```nix
{
  environment.systemPackages = [
    pkgs.firefox          # jailed
    pkgs.firefox.jailed   # jailed (explicitly)
    pkgs.firefox.unjailed # unjailed
  ]
}
```

---

## Examples

### Provide jail.nix as a module arg in a Nixos configuration:

It may be more ergonomic to provide the jail function as a module argument in
your Nixos configurations:

```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  outputs = { nixpkgs, jail-nix, ... }: {
    nixosConfigurations.my-nixos-host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ pkgs, ... }: { _module.args.jail = jail-nix.lib.init pkgs; })

        # Now all modules have `jail` passed into them:
        ({ jail, ... }: {
          environment.systemPackages = [
            (jail "jailed-hello" pkgs.hello [])
          ];
        })
      ];
    };
  };
}
```

### Using jail.nix to jail a flake package
```nix
# flake.nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.jail-nix.url = "sourcehut:~alexdavid/jail.nix";
  outputs = { nixpkgs, jail-nix, ... }: {
    packages.x86_64-linux =
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
        jail = jail-nix.lib.init pkgs;
      in {
        my-jailed-package = jail "jailed-hello" pkgs.hello [];
      };
  };
}
```

[![Build Status](https://travis-ci.org/maxhbr/myconfig.svg?branch=master)](https://travis-ci.org/maxhbr/myconfig)

Here I collect all configuration files of my machines. Currently I use
- OS: **[NixOS](https://nixos.org/)**
- Window Manager: **[xmonad](http://xmonad.org/)** (with: [xmobar](https://github.com/jaor/xmobar), [dmenu](https://tools.suckless.org/dmenu/) + [yeganesh](http://dmwit.com/yeganesh/))
- EDITOR: emacs ([spacemacs](http://spacemacs.org/))
- SHELL: zsh (with: [oh-my-zsh](http://ohmyz.sh/))
- Email Client: [neomutt](https://neomutt.org/) (with: [offlineimap](http://www.offlineimap.org/))
- Keyboard layout: [neo2](https://www.neo-layout.org/) 

# How to bootstrap
## on NixOS:
(first install NixOS, e.g. with [this guide](https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134))
```bash
$ git clone https://github.com/maxhbr/myconfig ~/myconfig
$ echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
$ ~/myconfig/rebuild.sh
```

## on other OS:
```bash
$ git clone https://github.com/maxhbr/myconfig ~/myconfig
$ ~/myconfig/xmonad/default.sh
$ ~/myconfig/dotfiles/default.sh
$ [...]
```

## just use the nixpkgs
```
$ export NIX_PATH=nixpkgs=/path/to/myconfig/nix/nixpkgs:nixpkgs-overlays=/path/to/myconfig/nix/overlays
```
or
```
$ export NIX_PATH=nixpkgs=/path/to/myconfig/nix/nixpkgs-unstable
```

The versions of the nixpkgs can be upgraded via `./nix/default.sh` and `./nix/nixpkgs-unstable/default.sh` (also called from `./nix/default.sh`).

# How it is composed

The top level script `rebuild.sh` calls the `prepare`, `deploy`, `upgrade` and `cleanup` phases for all `default.sh` files in the corresponding folders.

Some notable folders are
- `./scripts`, containing central scripts used to manage my system, e.g. for backups or automatic setup of xrandr configuration
  - this contains, among others, the scripts
    - `myautosetup.pl`, which sets up the environment depending on monitor configuration and more
    - `myborgbackup.sh`, my core backup script
    - `ec`, which calls emacs for me
- `./nix`, containing the configuration for nix, overlays, custom packages and also the `nixpkgs`
  - the nxpkgs at  `/nix/nixpkgs` are taken from the stable branch of nixos-latest via `git subtree`
    - it is by default updated updated, when `rebuild.sh` is ran
  - the `/nix/nixpkgs-unstable` contains a `default.nix` which pulls the package definitions on the fly
  - the `/nix/overlays` folder contains my overlays and some custom packages in `/nix/overlays/pkgs` (added as overlays)
- `./nixos`, containing the complete nixos configuration for multiple hosts (the hostname defines, which configuration is taken)
  - this imports
    - the folder `./xmonad` via the top level `./default.nix`
    - the configuration `./nix/nixpkgs-config.nix`
    - the overlays from `./nix/overlays`
    directly
- `./dotfiles`, containing some dotfiles, which are deployed via stow and should also work on non-NixOS Linux machines
  - for `emacs`, `git`, `mutt`, `shell` (e.g. `zsh`) and `core` (some scripts and more used frequently)
- `./xmonad`, my custom configuration of xmonad, packaged as lib via `nix` or `stack`
- `./background`, containing some self made desktop backgrounds

On top level there is also a file `./default.nix` which defines an overlay containing packages corresponding to the parts of myconfig.

On nixos the central folder is `./nixos`, which imports the top level overlay definition to also build xmonad, my backgrounds and more.

# License
This project is licensed under MIT (see [./LICENSE](./LICENSE))
```
SPDX-License-Identifier: MIT
```

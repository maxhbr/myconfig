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

## just use the my nixpkgs
```
$ export NIX_PATH=nixpkgs=/path/to/myconfig/nixpkgs
```

# Structure

My system configuration is managed by the script `./rebuild.sh` and is on NixOS packaged via nix.

## `./scripts`
this folder contains scripts, which I use to manage my system

## `./nixpkgs`
this contains the nixpkgs

## `./nixos`
this contains the nixos configuration of my machines.

## `./xmonad`
this contains my xmonad configuration.

## `./dotfiles`
this folder contains my dotfiles grouped by their topic. There is also a script
`deploy.sh` to deploy them via `stow`.

## `./background`
this contains desktop background images (all created by myself, CC-by-nd-4.0).

## `./misc`
just some miscellaneous stuff.

# License
This project is licensed under MIT (see [./LICENSE](./LICENSE))
```
SPDX-License-Identifier: MIT
```

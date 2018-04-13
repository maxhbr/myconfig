Here I collect all configuration files of my machines. Currently I use
- nixos
- xmonad
  - xmobar + yeganesh
- spacemacs (emacs + EVIL)
- neomutt
- zsh
- neo2 as keyboard layout

# How to bootstrap
## on NixOS:
```bash
$ git clone https://github.com/maxhbr/myconfig ~/myconfig
$ echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
$ cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee /etc/nixos/hostid
$ ~/myconfig/rebuild.sh
```

## on other OS:
```bash
$ git clone https://github.com/maxhbr/myconfig ~/myconfig
$ ~/myconfig/xmonad/default.sh
$ ~/myconfig/dotfiles/default.sh
$ [...]
```

# Folders
## `./dotfiles`
this folder contains my dotfiles grouped by their topic. There is also a script
`deploy.sh` to deploy them via `stow`.

## `./nixos`
this contains the nixos configuration of my machines.

## `./nix`
this contains the nixos configuration of nix.

## `./xmonad`
this contains my xmonad configuration.

## `./background`
this contains desktop background images (all created by myself, CC-by-nd-4.0).

## `./misc`
just some miscellaneous stuff.

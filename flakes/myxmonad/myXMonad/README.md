# My Xmonad Configuration

Currently my Xmonad Configuration is packaged via [nix](https://nixos.org/nix/) and deployed via the following lines in my nixos configuration (see [./../nixos/roles/desktop.nix]):
```nix
services.xserver.windowManager = {
  default = "myXMonad";
  session = [{
    name = "myXMonad";
    start = ''
      LOG=/tmp/myXmnad.log
      exec &> >(tee -a $LOG)
      ${pkgs.myconfig.my-xmonad}/bin/xmonad &
      waitPID=$!
    '';
  }];
};
```

Additionally, the configuration can be compiled via stack.

### The script `fast-rebuild.sh`
To replace the window manager without a whole `nixos-rebuild` on can try to run `fast-rebuild.sh`. But sadly this does not realy work right now.

### Examples and resources, which were used for inspiration:
- https://wiki.haskell.org/Xmonad/Config_archive/dmwit%27s_xmonad.hs
- https://wiki.haskell.org/Xmonad/Config_archive/adamvo's_xmonad.hs
- and many many more...

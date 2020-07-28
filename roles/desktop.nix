{ pkgs, ...}: {
  imports = [
    ./base.nix
    # modules
    # ../modules/desktop.Wayland.sway
    # ../modules/desktop.X.xfce.nix
    ../modules/desktop.X.xmonad.nix
    ../modules/pulseaudio
  ];
  config =
    { environment =
       { systemPackages = with pkgs;
           [ nfs-utils libnfs
           ];
       };

    };
}

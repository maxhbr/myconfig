# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/
{ system ? "x86_64-linux"
, hostConfig ? "roles/dev.nix" }:
let
  nixpkgs = ../nixpkgs;
  myisoconfig = { ... }: {
    imports = [
      "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
      "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
      (../. + "/${hostConfig}")
      ../modules/service.openssh.nix
    ];

    config = {
      networking.hostName = "myconfig";
      networking.wireless.enable = false;

      services.xserver.displayManager.lightdm.autoLogin =
        { enable = true;
          autoLogin.user = "mhuber";
        };

      # OpenSSH is forced to have an empty `wantedBy` on the installer system[1], this won't allow it
      # to be automatically started. Override it with the normal value.
      # [1] https://github.com/NixOS/nixpkgs/blob/9e5aa25/nixos/modules/profiles/installation-device.nix#L76
      systemd.services.sshd.wantedBy = lib.mkOverride 40 [ "multi-user.target" ];

      # add myconfig to iso
      isoImage.contents = [
        # { source = ./.; target = "myconfig/nixos"; }
        # { source = ../misc; target = "myconfig/misc"; }
        # { source = ../nixpkgs; target = "myconfig/nixpkgs"; }
        # { source = ../common.sh; target = "myconfig/common.sh"; }
        # { source = ../rebuild.sh; target = "myconfig/rebuild.sh"; }
        # { source = ../README.org; target = "myconfig/README.org"; }
        { source = ../LICENSE; target = "myconfig/LICENSE"; }
      ];
    };
  };

  evalNixos = configuration: import "${nixpkgs}/nixos" {
    inherit system configuration;
  };

in {
  iso = (evalNixos myisoconfig).config.system.build.isoImage;
}





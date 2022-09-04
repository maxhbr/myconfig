# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/nixos-hardware/common/pc/ssd
    {
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;

      networking.useDHCP = false;
      networking.interfaces.enp0s31f6.useDHCP = true;
      networking.interfaces.wlp3s0.useDHCP = true;
      networking.interfaces.wwp0s20f0u5c2.useDHCP = true;
    }
    ../../hardware/footswitch.nix
    ../../hardware/blink1.nix
    ../../hardware/unifying.nix
    ../host.x1extremeG2/hardware.hantek
    ../host.x1extremeG2/mykeylight
    ../host.x1extremeG2/role.work
    {
      services.xserver.windowManager.qtile.enable = true;
    }
  ];

  config = {
    networking.hostName = "p14";
    networking.hostId = "98234324";
    myconfig = {
      desktop.enable = true;
      virtualisation.enable = true;
      imagework.enable = true;
      cad.enable = true;
      deskreen.enable = true;
      dev = {
        compliance.enable = true;
        go.enable = true;
        haskell.enable = true;
        network.enable = true;
        nodejs.enable = true;
        # ruby.enable = true;
        rust.enable = true;
      };
    };
    virtualisation.docker.enable = true;
    virtualisation.podman.enable = true;
    # virtualisation.libvirtd.enable = true;

    services.xserver.wacom.enable = true;

    programs.sway.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_testing;
    boot.kernelPatches = [
      { name = "i915-P14sG3-intel-fix";
        patch = pkgs.writeTextFile {
          name = "i915-P14sG3-intel-fix";
          text = ''
diff --git a/drivers/gpu/drm/i915/display/intel_bios.c b/drivers/gpu/drm/i915/display/intel_bios.c
index 51dde5bfd..5dcd32cf9 100644
--- a/drivers/gpu/drm/i915/display/intel_bios.c
+++ b/drivers/gpu/drm/i915/display/intel_bios.c
@@ -2665,7 +2665,7 @@ static void parse_ddi_port(struct intel_bios_encoder_data *devdata)
 		drm_dbg_kms(&i915->drm,
 			    "More than one child device for port %c in VBT, using the first.\n",
 			    port_name(port));
-		return;
+		// return; // see https://gitlab.freedesktop.org/drm/intel/-/issues/5531#note_1477044
 	}

 	sanitize_device_type(devdata, port);
          '';
        };
      }
    ];

    hardware.enableRedistributableFirmware = true;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "21.05"; # Did you read the comment?
  };
}

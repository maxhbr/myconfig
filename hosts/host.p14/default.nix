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
          name = "i915-P14sG3-intel-fix.patch";
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
      { name = "i915-selective-fetch-fix";
        # https://patchwork.kernel.org/project/intel-gfx/patch/20220513142811.779331-2-jouni.hogander@intel.com/
        patch = pkgs.writeTextFile {
          name = "i915-selective-fetch-fix.patch";
          text = ''
diff --git a/drivers/gpu/drm/i915/display/intel_psr.c b/drivers/gpu/drm/i915/display/intel_psr.c
index 06db407e2749..fecdaaeac39e 100644
--- a/drivers/gpu/drm/i915/display/intel_psr.c
+++ b/drivers/gpu/drm/i915/display/intel_psr.c
@@ -1685,6 +1685,7 @@  static bool psr2_sel_fetch_pipe_state_supported(const struct intel_crtc_state *c
 int intel_psr2_sel_fetch_update(struct intel_atomic_state *state,
 				struct intel_crtc *crtc)
 {
+	struct drm_i915_private *dev_priv = to_i915(state->base.dev);
 	struct intel_crtc_state *crtc_state = intel_atomic_get_new_crtc_state(state, crtc);
 	struct drm_rect pipe_clip = { .x1 = 0, .y1 = -1, .x2 = INT_MAX, .y2 = -1 };
 	struct intel_plane_state *new_plane_state, *old_plane_state;
@@ -1770,6 +1771,19 @@  int intel_psr2_sel_fetch_update(struct intel_atomic_state *state,
 		clip_area_update(&pipe_clip, &damaged_area);
 	}

+	/*
+	 * TODO: For now we are just using full update in case
+	 * selective fetch area calculation fails. To optimize this we
+	 * should identify cases where this happens and fix the area
+	 * calculation for those.
+	 */
+	if (pipe_clip.y1 == -1) {
+		drm_info_once(&dev_priv->drm,
+			      "Selective fetch area calculation failed in pipe %c\n",
+			      pipe_name(crtc->pipe));
+		full_update = true;
+	}
+
 	if (full_update)
 		goto skip_sel_fetch_set_loop;
        '';
        };
      }
    ];

     home-manager.sharedModules = [{
       home.packages = with pkgs; [rdesktop];
     }];

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

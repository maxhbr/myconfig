# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

# see:
# - https://bugzilla.kernel.org/show_bug.cgi?id=206903#c1
# - https://wiki.gentoo.org/wiki/Ryzen#Random_reboots_with_mce_events#Random_reboots_with_mce_events
#
# $ dmesg | grep microcode
# ... kernel: mce: [Hardware Error]: CPU 4: Machine Check: 0 Bank 5: bea0000000000108
# ... kernel: mce: [Hardware Error]: TSC 0 ADDR 7f033cba0b1c MISC d012000200000000 SYND 4d000000 IPID 500b000000000
# ... kernel: mce: [Hardware Error]: PROCESSOR 2:870f10 TIME 1590853832 SOCKET 0 APIC a microcode 8701021

{ imports =
    [ { home-manager.users.mhuber =
          { home.packages = with pkgs; [mcelog];
          };
        hardware.mcelog.enable = true;
      }
      { config =
          { nixpkgs.overlays = [(self: super:
              { firmwareLinuxNonfree = super.firmwareLinuxNonfree.overrideAttrs ( oldAttrs:
                  let
                    version = "2020-05-19";
                  in
                    { inherit version;
                      src = self.fetchgit
                        { url = "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git";
                          rev = self.lib.replaceStrings ["-"] [""] version;
                          sha256 = "13yrpgfqxp5l457p3s1c61is410nv0kv6picx9r0m8h1b0v6aym3";
                        };
                      outputHash = "0pjl70nwarnknxah8vikb051c75mkg25a5m4h3344cw86x8hcx10";
                    });
               })];
          };
      }
    ];
  config =
    { boot.kernelParams =
        [
          #"amdgpu.dpm=0"
                # PP_SCLK_DPM_MASK             = 1
                # PP_MCLK_DPM_MASK             = 1
                # PP_PCIE_DPM_MASK             = 0   This is PCIe Dynamic Power Managment..
                # PP_SCLK_DEEP_SLEEP_MASK      = 1
                # PP_POWER_CONTAINMENT_MASK    = 1
                # PP_UVD_HANDSHAKE_MASK        = 1
                # PP_SMC_VOLTAGE_CONTROL_MASK  = 1
                # PP_VBI_TIME_SUPPORT_MASK     = 1
                # PP_ULV_MASK                  = 1
                # PP_ENABLE_GFX_CG_THRU_SMU    = 1
                # PP_CLOCK_STRETCH_MASK        = 1
                # PP_OD_FUZZY_FAN_CONTROL_MASK = 1
                # PP_SOCCLK_DPM_MASK           = 1
                # PP_DCEFCLK_DPM_MASK          = 1
                # PP_OVERDRIVE_MASK            = 1
                # PP_GFXOFF_MASK               = 1
                # PP_ACG_MASK                  = 1
                # PP_STUTTER_MODE              = 1
                # PP_AVFS_MASK                 = 1
          # "amdgpu.ppfeaturemask=0xfffffffb"
          "amdgpu.ppfeaturemask=0xffffbffb" # PP_SCLK_DPM_MASK=1 PP_MCLK_DPM_MASK=0
          # "amdgpu.ppfeaturemask=0xffffbffe" # PP_SCLK_DPM_MASK=0 PP_MCLK_DPM_MASK=1
        ];
    };
}

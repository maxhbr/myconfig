{ lib, pkgs, ... }: {
  imports = [ ../../../common/cpu/intel ../../../common/pc/laptop ];

  # Includes the Wi-Fi and Bluetooth firmware for the QCA6390.
  hardware.enableRedistributableFirmware = true;

  # Wi-Fi currently requires a specific set of patches to function.
  # To ensure these patches do not conflict, we pin to a specific version here.
  # TODO: Remove this once patches have finally landed in mainline.
  boot.kernelPackages = let
    linux_patched_pkg =
      { buildLinux, fetchurl, modDirVersionArg ? null, ... }@args:
      buildLinux (args // rec {
        version = "5.10.18";
        modDirVersion = if (modDirVersionArg == null) then
          builtins.replaceStrings [ "-" ] [ ".0-" ] version
        else
          modDirVersionArg;
        src = fetchurl {
          url = "mirror://kernel/linux/kernel/v5.x/linux-${version}.tar.xz";
          sha256 = "04dnkg5j73f6cd8ws1prrrjx37srz7rm66bj6slmnfzp3cmyxh9v";
        };

        # kvalo qca6390 patches
        # https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/log/?h=ath11k-qca6390-bringup
        kernelPatches = [
          {
            name = "add-64-bit-check-before-reading-msi-high-addr";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=065c9528cc508cfbf6e3399582df29f76f56163c";
              sha256 = "1mqhwags919vlxllzqh5kj4b2l869swvfwa89jk804a1l4l02fmv";
            };
          }
          {
            name = "pci-support-platforms-with-one-msi-vector";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=59c6d022df8efb450f82d33dd6a6812935bd022f";
              sha256 = "0sxbb58bnryb9hic1cyc8dzrzachhca7a6hywyzz1pksh9syhs5y";
            };
          }
          {
            name = "fix-monitor-status-dma-unmap-direction";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=fa4eea695afb286ae38beb30dabf251335cb4a62";
              sha256 = "1sh3d8ck4nlg671j2y8f07394xrqlnbrvh9rmy4l1zfpz7wa7d10";
            };
          }
          {
            name = "hook-mhi-suspend-and-resume";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=762fe5bc2dd1e43ef307a375861b1a8c414b14e3";
              sha256 = "154p8gp4smmmkhyx127f6rib04xd5bn38a3n4893rbyyb5kckv40";
            };
          }
          {
            name = "implement-hif-suspend-and-resume-functions";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=2f164833bcca14e8aec0b2566eae4b5a7d09ee6f";
              sha256 = "1ic968y1ivlgfhbj67ds809zqas7n50kc6wb8jgksk227dvagnip";
            };
          }
          {
            name = "read-select_window-register-to-ensure-write-is-finished";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=6afab932ece78fedc1538c20c2aefdd13aa6c9d0";
              sha256 = "19jiz9mf868rj57ljjdb3n97sfi6x78ac9kgd7fhg1bh0zjjiskp";
            };
          }
          {
            name = "implement-htc-suspend-related-callbacks";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=69ab2835b82c176e793195243e1400d4f8db3647";
              sha256 = "1gba5h0s6c6zjplw8zyqc2qj21ly1m2xzbgznml159wzj2xvzb2m";
            };
          }
          {
            name = "put-target-to-suspend-when-system-enters-suspend-state";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=68023bee4d61ea2b02af49bba00adabba51d8b6b";
              sha256 = "05aqdjd5xps0wncrh41r805fn2rpnhw53pn02a374g81bbifwa5q";
            };
          }
          {
            name = "pci-print-a-warning-if-firmware-crashed";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=23dcef9436560a033703164c4daff9e36e640969";
              sha256 = "0m45wvilr2cgdkpdjzcz4hdzsfs596ibjsvd7sdksjbrp5wslla1";
            };
          }
          {
            name = "qmi-print-allocated-memory-segment-addresses-and-sizes";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=a327caa4a5a677161a6f1d29514e8cb42236e956";
              sha256 = "1id6xz7siw1x2xa00psqvr4h5zb0xd83apy0cyv4jqzkd5x1kwl0";
            };
          }
          {
            name = "put-hw-to-dbs-using-mode";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=ce8b5dfc16a0b84ac9ab2d508c2d5e66e8bf179a";
              sha256 = "0gcnzn82mjdqy3ly494xfawqb9xvwd01dcdr43cw8ik92jggs4sf";
            };
          }
          {
            name = "fix-pcie-link-unstable-issue";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=a82a3aee7cde95d533c28cad3749e3c354011896";
              sha256 = "0r26g7j7kkm76bippp79vd462ykc8k8p0bxr7pshhkyazs6v1ij3";
            };
          }
          {
            name = "fix-pci-l1ss-clock-unstable-problem";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=08816aab67540e6babc558dafa973fc905a9afa1";
              sha256 = "180hp6iwgw7cqiiwhp9cnzwr5z9n26pphi2j693x751crzr0xkzw";
            };
          }
          {
            name = "disable-otp-write-privilege";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=86c5a1d6983e647a55448c80f94eb8f0aa97dfad";
              sha256 = "176g07kpsqnkc3vpfx2lhlrksmdg05m0zxn1i5yfvksczp2215iq";
            };
          }
          {
            name = "disable-aspm-l0sls-before-downloading-firmware";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=8bd374e3305359ca0be9fe88e8a1edc1abd537eb";
              sha256 = "1grjsf6jvn536cz6wil79l2lzc90ga1c7sisv9j0qac7jzr7x5rz";
            };
          }
          {
            name = "purge-rx-pktlog-when-entering-suspend";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=6f481de563dd108bd3df616c80e60f308b7a48e3";
              sha256 = "14qd1qv8v3mcslj7crzrw0ib1caa7vbnq7jkq163248658bbmk6p";
            };
          }
          {
            name = "set-credit_update-flag-for-flow-controlled-ep-only";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=6e0fba395a054cd58d87b3749f1f4ff2f3fef92e";
              sha256 = "0sgjxj6m3fdlgdfg7rv5fajfmbmrccy5asrammlgbc7gh5sn9ac4";
            };
          }
          {
            name = "implement-wow-enable-and-wow-wakeup-command";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=cfca935c92d8f2b31c95e7fd074645245f54492a";
              sha256 = "0dwbsqkw3f8v676s0x3jv04w0qk36ypvnwh02rx4qfdk38sh0j3j";
            };
          }
          {
            name = "add-ce-irq-enable-and-disable-hif-layer-functions";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=9297794a5d5af5e82b9554677f959add281a5b76";
              sha256 = "0hl93l8khh36drllxii969nvkb6p4hh28gnjyg0y10adm5q9b4ac";
            };
          }
          {
            name = "put-target-to-wow-state-when-suspend-happens";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=d448ef6decab05c499ffc005c56307a6fc1f1abe";
              sha256 = "17pgbmsryg96626xpbkzd0c27z71lsy7ygli4c6d4dzk5b9594zn";
            };
          }
          {
            name = "vdev-delete-sync-with-fw";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=c1d3ee50859a2d2c132a8461fdabde568df5ee20";
              sha256 = "1dscfdqv5x3h024gryh0464mky0j6z681rliiix17kdh172vxx52";
            };
          }
          {
            name = "peer-delete-sync-with-fw";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=6244933ddba318b36bb00c48eeb8d63a24a901c2";
              sha256 = "0p1663w0lik44gwyfzmxxiwnc3s9n3p46aappla8pbfk9wdgw86d";
            };
          }
          {
            name = "start-vdev-if-bss-peer-already-created";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=c58d077298d91c61c2466a50b58181a234474381";
              sha256 = "1462w29isnlfqs4bavzprhj48wbzvmwasbzh4djzfnsjm0ld7z90";
            };
          }
          {
            name = "hack-mhi-disable-m2-state";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=cdda596d45a99fed1fe74b0605de6b220c955c0b";
              sha256 = "0wfmaygzl8fav8lr67pjnhqskm6mh9ykcwqh71ijxvfn0kbq9fqg";
            };
          }
          {
            name = "hack-revert-place-pages-to-tail";
            patch = pkgs.fetchpatch {
              url =
                "https://git.kernel.org/pub/scm/linux/kernel/git/kvalo/ath.git/patch/?id=60fad49a69e7b2f896ce7b1ade4ed532227b8e22";
              sha256 = "0zhzjyym42r3rjwh55vk6p423lhz7555mb7xjqk63lczrsc221nm";
            };
          }

          # Extra config required for Bluetooth.
          # NOTE: Should consider upstreaming this to the default nix config.
          # Especially the `SERIAL_DEV_BUS` and `SERIAL_DEV_CTRL_TTYPORT`
          # options, as these are the recommended defaults.
          {
            name = "enable-qca6390-bluetooth";
            patch = null;
            extraConfig = ''
              BT_QCA m
              BT_HCIUART m
              BT_HCIUART_QCA y
              BT_HCIUART_SERDEV y
              SERIAL_DEV_BUS y
              SERIAL_DEV_CTRL_TTYPORT y
            '';
          }
        ];

        # Enable some extra kernel modules for QCA6390 bluetooth.
        kernelModules = [ "btqca" "hci_qca" "hci_uart" ];
      } // (args.argsOverride or { }));
    linux_patched = pkgs.callPackage linux_patched_pkg { };
  in pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor linux_patched);

  # Touchpad goes over i2c.
  # Without this we get errors in dmesg on boot and hangs when shutting down.
  boot.blacklistedKernelModules = [ "psmouse" ];

  # Allows for updating firmware via `fwupdmgr`.
  services.fwupd.enable = true;
}

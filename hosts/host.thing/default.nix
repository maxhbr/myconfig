# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/btrfs.nix
    inputs.nixos-hardware.nixosModules.framework-desktop-amd-ai-max-300-series
    {
      myconfig.observability = {
        host_hostname = "nuc";
        client = {
          enable = true;
          enableDcgmExporter = true;
          dcgmExporterUseContainer = true;
        };
      };
    }
    ./nvidia.dcgm-exporter.nix
    (myconfig.metadatalib.fixIp "enp191s0")
    (myconfig.metadatalib.setupAsBuildMachine [
      myconfig.metadatalib.get.hosts.p14.pubkeys."id_ed25519_no_pw.pub"
    ])
    { environment.systemPackages = with pkgs; [ linuxPackages.usbip ]; }
    {
      # programs.mosh.enable = lib.mkDefault true;
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
    ./programs.opencode.nix
    ../shared.deployedServices.nix
    ./services.caddy.nix
    ./services.forgejo.nix
    ./services.gitolite.nix
    (
      { ... }:
      {
        myconfig.ai.localModels = [
          {
            port = 22546;
          }
          {
            port = 22545;
          }
        ];
      }
    )
    # ./services.ollama.nix
    ./myconfig.ai.llama-cpp
    ./services.open-webui.nix
    ./services.wyoming.nix
    ./services.qdrant.nix
    ./specialisation.nix
    ./hardware.Radeon8060S.nix
    ./disk.nix
    ./btr2-pool.nix
    ../../hardware/eGPU.nix
    {
      home-manager.sharedModules = [
        {
          services.swayidle.enable = lib.mkForce false;
          services.wayvnc.enable = true;
        }
      ];
    }
    {
      security.pam.loginLimits = [
        {
          domain = "@wheel";
          item = "memlock";
          type = "soft";
          value = "unlimited";
        }
        {
          domain = "@wheel";
          item = "memlock";
          type = "hard";
          value = "unlimited";
        }
      ];
    }
  ];

  config = {
    networking.hostName = "thing";
    networking.hostId = "3e7c8a1f";
    networking.useDHCP = false;
    system.nixos.tags = config.myconfig.hardware.gpu.variant;

    services.litellm = {
      enable = true;
      # Bind LiteLLM to all interfaces so peers (e.g. p14, f13) can reach
      # http://<thing-wg-ip>:4000/v1 directly. The host itself (Caddy,
      # open-webui, vmagent, …) reaches it through `localhost:4000`. The
      # firewall rule below restricts external exposure to wg0. This
      # overrides the 127.0.0.1 lib.mkForce in
      # modules/myconfig.ai/services.litellm.nix.
      host = lib.mkOverride 49 "0.0.0.0";
      settings.router_settings = {
        model_group_alias = {
          "hermes" = "rtx5090:hermes";
          "hermes-fallback" = "rtx5090:hermes-fallback";
          "opencode-fast" = "rtx5090:opencode-fast";
          "opencode-fast-fallback" = "rtx5090:opencode-fast-fallback";
          "opencode" = "gfx1151:opencode";
          "opencode-slow" = "gfx1151:opencode-slow";
          "opencode-fallback" = "gfx1151:opencode-fallback";
          "sidekick" = "rtx5090:sidekick";
        };
      };
    };

    # Allow peers to reach LiteLLM on port 4000 via wg0.
    networking.firewall.interfaces."wg0".allowedTCPPorts = [
      config.services.litellm.port
    ];

    myconfig = {
      persistence.impermanence = {
        enable = true;
        soft_permanence_for_boot = false;
        tmpfs_size = "20%";
        btrfs_device = "/dev/mapper/enc-pv";
      };
      headless.enable = true;
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          selectedSessions = [
            "niri"
            # "labwc"
          ];
          directLoginFirstSession = true;
        };
        imagework.enable = true; # https://github.com/NixOS/nixpkgs/issues/425306
        imagework.myphoto.enable = true;
      };
      ai = {
        enable = true;
        pi-coding-agent.enable = true;
        pull_models = {
          enable = true;
          # Specs without a corresponding `myconfig.ai.llama-cpp.models`
          # entry stay declared here. Everything that *is* served by
          # llama-cpp is collected automatically from each model's
          # `pull-models = { target_directory; hf_spec; }` (see
          # ./myconfig.ai.llama-cpp.nix).
          models = {
            "/home/mhuber/models" = [
              "unsloth/Qwen3.6-27B-NVFP4"
              "sakamakismile/Qwen3.6-27B-Text-NVFP4-MTP"
            ];
          };
        };
        inference-cpp = {
          enable = true;
        };
        lmstudio = {
          enable = true;
        };
        alpaca = {
          enable = false;
        };
        # open-webui = {
        #   enable = true;
        # };
        comfyui = {
          enable = true;
        };
        container = {
          crawl4ai = {
            enable = false;
          };
        };
      };
      containers.n8n.enable = true;
      dev.core.enable = true;
      virtualisation.enable = true;
    };

    virtualisation = {
      podman.enable = true;
      oci-containers = {
        backend = "podman";
      };
      libvirtd.enable = true;
    };

    services.hardware.bolt.enable = true;

    hardware.enableRedistributableFirmware = true;

    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
      initrd = {
        supportedFilesystems = [
          "btrfs"
          "luks"
          "nfs"
        ];
        kernelModules = [ "nfs" ];
      };
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}

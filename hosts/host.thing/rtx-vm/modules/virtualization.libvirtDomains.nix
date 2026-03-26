# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.virtualisation.libvirtDomains.ai-gpu-vm;
in
{
  options.virtualisation.libvirtDomains.ai-gpu-vm = {
    enable = lib.mkEnableOption "Ubuntu AI VM with GPU passthrough";

    memory = lib.mkOption {
      type = lib.types.int;
      default = 16384; # 16 GB
      description = "Memory in MB";
    };

    vcpus = lib.mkOption {
      type = lib.types.int;
      default = 6;
      description = "Number of virtual CPUs";
    };

    diskSize = lib.mkOption {
      type = lib.types.str;
      default = "100G";
      description = "Virtual disk size";
    };

    storagePool = lib.mkOption {
      type = lib.types.str;
      default = "default";
      description = "Libvirt storage pool name";
    };

    networkType = lib.mkOption {
      type = lib.types.enum [
        "nat"
        "bridge"
      ];
      default = "nat";
      description = "Network type";
    };

    gpuPassthrough = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable GPU passthrough";
    };

    ubuntuVersion = lib.mkOption {
      type = lib.types.str;
      default = "24.04";
      description = "Ubuntu version";
    };

    customStoragePath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "/mnt/disk/virtlibvirt/images";
      description = "Custom storage path for VM disks";
    };
  };

  config = lib.mkIf cfg.enable {
    # Create custom storage directory if specified
    systemd.tmpfiles.rules = lib.optional (
      cfg.customStoragePath != null
    ) "d ${cfg.customStoragePath} 0755 root root -";

    # Libvirt domain definition via systemd service (creates XML definition)
    systemd.services.libvirt-define-ai-gpu-vm = {
      description = "Define AI GPU VM with passthrough";
      wantedBy = [ "multi-user.target" ];
      after = [ "libvirtd.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;

      script =
        let
          vmName = "ai-gpu-vm";
          diskPath =
            if cfg.customStoragePath != null then
              "${cfg.customStoragePath}/${vmName}.qcow2"
            else
              "/var/lib/libvirt/images/${vmName}.qcow2";
        in
        ''
                    VM_XML="/etc/libvirt/qemu/${vmName}.xml"

                    # Check if VM already defined
                    if virsh dominfo ${vmName} >/dev/null 2>&1; then
                      echo "VM ${vmName} already defined, skipping"
                      exit 0
                    fi

                    # Create VM XML definition
                    cat > "$VM_XML" <<EOF
          <domain type='kvm'>
            <name>${vmName}</name>
            <memory unit='MiB'>${toString cfg.memory}</memory>
            <currentMemory unit='MiB'>${toString cfg.memory}</currentMemory>
            <vcpu placement='static'>${toString cfg.vcpus}</vcpu>

            <features>
              <acpi/>
              <apic/>
              <hyperv>
                <relaxed state='on'/>
                <vapic state='on'/>
                <spinlocks state='on' retries='8191'/>
              </hyperv>
            </features>

            <cpu mode='host-passthrough' check='none'>
              <topology sockets='1' dies='1' cores='${toString cfg.vcpus}' threads='1'/>
            </cpu>

            <os>
              <type arch='x86_64' machine='q35'>hvm</type>
              <loader readonly='yes' type='pflash'>/usr/share/OVMF/OVMF_CODE.fd</loader>
              <nvram template='/usr/share/OVMF/OVMF_VARS.fd'>/var/lib/libvirt/qemu/nvram/${vmName}_VARS.fd</nvram>
              <boot dev='hd'/>
            </os>

            <devices>
              <emulator>/run/current-system/sw/bin/qemu-system-x86_64</emulator>

              <!-- Disk -->
              <disk type='file' device='disk'>
                <driver name='qemu' type='qcow2' cache='none' io='native'/>
                <source file='${diskPath}'/>
                <target dev='vda' bus='virtio'/>
                <boot order='1'/>
              </disk>

              <!-- Network -->
              <interface type='${cfg.networkType}'>
                <source ${if cfg.networkType == "nat" then "network='default'" else "bridge='br0'"} />
                <model type='virtio'/>
              </interface>

              <!-- Graphics (for initial install, disabled after GPU passthrough) -->
              <graphics type='spice' port='5900' autoport='yes'>
                <listen type='address'/>
              </graphics>
              <video>
                <model type='qxl' ram='65536' vram='65536' vgamem='16384' heads='1'/>
              </video>

              ${
                if cfg.gpuPassthrough then
                  ''
                    # GPU Passthrough - NVIDIA RTX 5090 (IOMMU Group 17)
                    <hostdev mode='subsystem' type='pci' managed='yes'>
                      <source>
                        <address domain='0x0000' bus='0x62' slot='0x00' function='0x0'/>
                      </source>
                      <rom bar='on'/>
                      <address type='pci' domain='0x0000' bus='0x01' slot='0x00' function='0x0'/>
                    </hostdev>

                    <hostdev mode='subsystem' type='pci' managed='yes'>
                      <source>
                        <address domain='0x0000' bus='0x62' slot='0x00' function='0x1'/>
                      </source>
                      <address type='pci' domain='0x0000' bus='0x01' slot='0x00' function='0x1'/>
                    </hostdev>
                  ''
                else
                  ""
              }

              <!-- Input devices -->
              <input type='tablet' bus='usb'/>
              <input type='mouse' bus='ps2'/>
              <input type='keyboard' bus='ps2'/>

              <!-- Audio -->
              <sound model='ich9'>
                <address type='pci' domain='0x0000' bus='0x00' slot='0x1b' function='0x0'/>
              </sound>

              <!-- USB controller -->
              <controller type='usb' index='0' model='qemu-xhci'>
                <address type='pci' domain='0x0000' bus='0x02' slot='0x00' function='0x0'/>
              </controller>

              <!-- TPM (optional) -->
              <tpm model='tpm-tis'>
                <backend type='emulator' version='2.0'/>
              </tpm>

              <!-- Watchdog -->
              <watchdog model='itco' action='reset'/>

              <!-- Console -->
              <channel type='unix'>
                <source mode='bind'/>
                <target type='virtio' name='org.qemu.guest_agent.0'/>
              </channel>
            </devices>
          </domain>
          EOF

                    # Define the VM
                    virsh define "$VM_XML"

                    # Create disk if it doesn't exist
                    if [ ! -f "${diskPath}" ]; then
                      echo "Creating disk image at ${diskPath}"
                      mkdir -p "$(dirname "${diskPath}")"
                      qemu-img create -f qcow2 "${diskPath}" "${cfg.diskSize}"
                    fi

                    echo "VM ${vmName} defined successfully"
        '';
    };

    # NixOS user group for libvirt
    users.groups.libvirt = lib.mkForce {
      gid = null;
    };
  };
}

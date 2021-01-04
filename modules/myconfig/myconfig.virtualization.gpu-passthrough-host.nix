{ config, lib, pkgs, ... }@args:
let
  cfg = config.myconfig;
  user = cfg.user;

  list_IOMMU_groups = pkgs.writeShellScriptBin "list_IOMMU_groups" ''
shopt -s nullglob
for d in /sys/kernel/iommu_groups/*/devices/*; do
    n=''${d#*/iommu_groups/*}; n=''${n%%/*}
    printf 'IOMMU Group %s ' "$n"
    lspci -nns "''${d##*/}"
done;
'';
in {
  options.myconfig = with lib; {
    virtualisation.gpuPassthroughHost.enable = mkEnableOption "myconfig.virtualisation.gpuPassthroughHost";
  };
  config = (lib.mkIf cfg.virtualisation.gpuPassthroughHost.enable (
    {
      boot.kernelParams = [
        "amd_iommu=on"
        "pcie_aspm=off"
        "pcie_acs_override=downstream,multifunction"
        "vfio_virqfd" "vfio_pci" "vfio_iommu_type1" "vfio"
      ];
      boot.initrd.preDeviceCommands = ''
  DEVS="0000:0b:00.0 0000:0b:00.1"
  for DEV in $DEVS; do
    echo "vfio-pci" > /sys/bus/pci/devices/$DEV/driver_override
  done
  modprobe -i vfio-pci
'';
      boot.blacklistedKernelModules = [ "amdgpu" "nvidia" "nouveau" ];
      environment = {
        systemPackages = with pkgs; [
          list_IOMMU_groups
        ];
      };
    } // (import ../../nixpkgs/nixos/modules/profiles/headless.nix args)
  ));
}

{ pkgs, config, lib, ... }: {
  config = (lib.mkIf config.virtualisation.libvirtd.enable {
    environment.systemPackages = with pkgs;
      [ qemu virt-manager ] ++ (with nixos-unstable; [ nixos-shell ]);
    boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
    boot.extraModprobeConfig =
      "options kvm_intel nested=1"; # enable nested virtualization

    virtualisation.libvirtd = {
      qemu = {
        ovmf.enable = true;
        runAsRoot = false;
      };
      onBoot = "ignore";
      onShutdown = "shutdown";
    };
  });
}

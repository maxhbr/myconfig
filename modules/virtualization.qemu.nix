{ pkgs, config, lib, ... }: {
  config = (lib.mkIf config.virtualisation.libvirtd.enable {
    environment.systemPackages = with pkgs;
      [ qemu aqemu
        virtmanager
      ] ++
      (with unstable; [ nixos-shell ]);
    boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
    boot.extraModprobeConfig = "options kvm_intel nested=1"; # enable nested virtualization
  });
}

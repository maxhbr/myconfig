{ pkgs, config, lib, ... }: {
  config = (lib.mkIf config.virtualisation.libvirtd.enable {
    environment.systemPackages = with pkgs;
      [ qemu aqemu ] ++
      (with unstable; [ nixos-shell ]);
    boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
  });
}

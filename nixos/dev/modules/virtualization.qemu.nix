{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [
      qemu aqemu
    ];
  };
}

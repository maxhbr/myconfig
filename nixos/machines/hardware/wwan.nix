{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    libqmi
    libmbim
    rfkill
  ];
  systemd.services.ModemManager.enable = true;
  nixpkgs.overlays = [(pkgsself: pkgssuper: {
    modemmanager = pkgssuper.modemmanager.overrideDerivation (super: rec {
      # wait for: https://github.com/NixOS/nixpkgs/commit/40312b6a9b8b4d20b825520e17efaa543300e772
      name = "ModemManager-${version}";
      version = "1.6.8";
      src = pkgssuper.fetchurl {
        url = "http://www.freedesktop.org/software/ModemManager/${name}.tar.xz";
        sha256 = "0xj3ng7qcqxkib5qkprwghcivaz0mn449fw08l67h1zbpz23bh7z";
      };
    });
  })];
}

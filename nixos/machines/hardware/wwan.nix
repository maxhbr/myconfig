{ config, pkgs, ... }:

let
  unstable = (import <unstable> {});
in {
  # - https://askubuntu.com/questions/747959/sierra-e7455-mobile-broadband-modem
  #   > The Lenovo variant of the EM7455 has a feature we know as "FCC_AUTH" turned on. This disables the radio until the driver sends a "magic message" to the modem. This feature is well known from older Sierra modems. ModemManager supports the magic message for modems in QMI mode, but we have so far got away with ignoring it in MBIM mode. The EM7455 changes that...
  #   >
  #   > Basically we know a way around the issue but it isn't yet implemented in current libmbim/ModemManager versions. Sorry about that. I'm sure this will be fixed soon, being a show stopper for the Lenovo EM7455.
  #   >
  #   > See the discussion here for full details: https://lists.freedesktop.org/archives/libmbim-devel/2016-April/000703.html
  #   - See: https://forums.lenovo.com/t5/Linux-Discussion/ThinkPad-X1-Yoga-LTE-Sierra-EM7455-on-Linux-Ubuntu-16-04/m-p/3371464/highlight/true#M7978
  # see also:
  # - https://wiki.archlinux.org/index.php/ThinkPad_mobile_internet
  #   - https://raw.githubusercontent.com/penguin2716/qmi_setup/master/qmi_setup.sh
  # - https://bugzilla.redhat.com/show_bug.cgi?id=1379406
  # - supported devices: https://www.freedesktop.org/wiki/Software/ModemManager/SupportedDevices/#index20h1
  # - https://forums.lenovo.com/t5/Linux-Discussion/ThinkPad-X1-Yoga-LTE-Sierra-EM7455-on-Linux-Ubuntu-16-04/td-p/3344299/page/2
  environment.systemPackages = with pkgs; [
    libqmi
    libmbim
    # wvdial
    # modemmanager
    # libsForQt5.modemmanager-qt
    rfkill

    # pkgs.em7455-udev-rules
  ];
  systemd.services.ModemManager.enable = true;
  nixpkgs.overlays = [(pkgsself: pkgssuper: {
    modemmanager = pkgssuper.modemmanager.overrideDerivation (super: rec {
      name = "ModemManager-${version}";
      version = "1.6.6";
      src = pkgssuper.fetchurl {
        url = "http://www.freedesktop.org/software/ModemManager/${name}.tar.xz";
        sha256 = "0hwalp5ixg4b1qdj3ni61nkvjlxclb7zw7j548pwpmqdkq6sll5h";
      };
    });
    # pkgssuper.callPackage ../../../nix/pkgs/modemmanager {};
    # inherit (unstable) libproxy networkmanager networkmanagerapplet libqmi libmbim; # modemmanager;
  })];
  # networking.networkmanager.packages = [];

  # networking.networkmanager.basePackages = with pkgs; {
  #   inherit networkmanager modemmanager wpa_supplicant
  #           networkmanager_openvpn networkmanager_vpnc
  #           networkmanager_openconnect # networkmanager_fortisslvpn
  #           networkmanager_pptp networkmanager_l2tp; };
}

{ config, pkgs, ... }:

{
  # see:
  # - https://wiki.archlinux.org/index.php/ThinkPad_mobile_internet
  #   - https://raw.githubusercontent.com/penguin2716/qmi_setup/master/qmi_setup.sh
  # - https://askubuntu.com/questions/747959/sierra-e7455-mobile-broadband-modem
  #   - https://lists.freedesktop.org/archives/libmbim-devel/2016-April/000703.html
  environment.systemPackages = with pkgs; [
    libqmi
  ];
}

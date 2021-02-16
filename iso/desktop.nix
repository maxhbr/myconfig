import ./mkiso.nix {
  isoConfig = {
    myconfig = {
      desktop.enable = true;
      virtualisation.enable = true;
    };

    services.vsftpd.enable = true;
  };
}

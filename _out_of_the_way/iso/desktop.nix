import ./mkiso.nix {
  customModule = { ... }: {
    myconfig = {
      desktop.enable = true;
      virtualisation.enable = true;
    };

    services.vsftpd.enable = true;
  };
}

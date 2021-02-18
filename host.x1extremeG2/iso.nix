import ../iso/mkiso.nix {
  customModule = {...}: {
    imports = [ ./role.work ];
    myconfig = {
      desktop.enable = true;
      virtualisation.enable = true;
      imagework.enable = true;
      dev = {
        network.enable = true;
        compliance.enable = true;
      };
    };
    services.vsftpd.enable = true;
  };
}

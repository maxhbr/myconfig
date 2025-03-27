{ pkgs, config, inputs, ... }: {
  imports = [
    ./RTX5090.nix
    # ./RX5500XT.nix
  ];
  config = {
    # boot.kernelParams = [ "pcie_aspm=off" ];

    # services.tlp.settings = {
    #   RUNTIME_PM_DRIVER_BLACKLIST = "nouveau nvidia radeon amdgpu";
    #   RUNTIME_PM_ON_AC = "on";
    #   RUNTIME_PM_ON_BAT = "on";
    # };

    # services.udev.extraRules = ''
    #   ACTION=="add", SUBSYSTEM=="pci", 
    #   ATTR{vendor}=="0x10de",  # NVIDIA vendor ID
    #   ATTR{power/control}="on"
    #   ACTION=="add", SUBSYSTEM=="pci",
    #   ATTR{vendor}=="0x1002",  # AMD vendor ID
    #   ATTR{power/control}="on"
    # '';
  };
}


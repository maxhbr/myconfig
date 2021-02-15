{ pkgs, config, lib, ... }:
let cfg = config.myconfig.dev.compliance;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      (import ../../pkgs/license-compliance-toolbox).packages.x86_64-linux.license-compliance-toolbox
    ];
  };
}

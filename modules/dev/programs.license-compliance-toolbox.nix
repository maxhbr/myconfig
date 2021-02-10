{ pkgs, config, lib, ... }:
let cfg = config.myconfig.dev.compliance;
    flake = pkgs.getFlake (toString ../../../MINE/license-compliance-toolbox);
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.callPackage ../../pkgs/license-compliance-toolbox { inherit pkgs; })
      (flake.license-compliance-toolbox)
      # (pkgs.callPackage ../../pkgs/oss-review-toolkit-ort { })
      # (pkgs.callPackage ../../pkgs/license-compliance-toolbox/scancode-workbench-git.nix { })
      (pkgs.callPackage ../../pkgs/scanoss-scanner { })
    ];
  };
}

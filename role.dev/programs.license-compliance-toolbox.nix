{ pkgs, ... }: {
  config = {
    environment.systemPackages = [
      (pkgs.callPackage ../pkgs/license-compliance-toolbox { inherit pkgs; })
      # (pkgs.callPackage ../pkgs/license-compliance-toolbox/scancode-workbench-git.nix { })
    ];
  };
}

{ pkgs, ... }: {
  config = {
    environment.systemPackages = [
      (pkgs.callPackage ../pkgs/license-compliance-toolbox { inherit pkgs; })
    ];
  };
}

self: super: {
  microcodeIntel = self.callPackage ../pkgs/microcode/intel.nix {pkgs = super;};
}

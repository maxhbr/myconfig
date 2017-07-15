self: super:
let
  # unstable = (import <unstable> {});
  mkEnv = name: paths: self.buildEnv {
    inherit name paths;
    ignoreCollisions = true;
  };
in  {
  ebookEnv = mkEnv "ebookEnv" [
    super.k2pdfopt
    super.calibre
  ];
  ideEnv = mkEnv "ideEnv" [
    super.jetbrains.phpstorm
    super.jetbrains.idea-ultimate
  ];
}

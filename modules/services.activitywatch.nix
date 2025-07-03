# see: https://github.com/NixOS/nixpkgs/issues/97379#issuecomment-1228258543
{ config, inputs, ... }:
let
  cfg = config.myconfig;
  hasWayland = cfg.desktop.wayland.enable;
in
{
  home-manager.sharedModules = [
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        awatcher = pkgs.rustPlatform.buildRustPackage rec {
          pname = "awatcher";
          version = "main";
          src = inputs.awatcher-src;

          cargoLock = {
            lockFile = "${inputs.awatcher-src}/Cargo.lock";
            outputHashes = {
              "aw-client-rust-0.1.0" = "sha256-fCjVfmjrwMSa8MFgnC8n5jPzdaqSmNNdMRaYHNbs8Bo=";
            };
          };

          nativeBuildInputs = [ pkgs.pkg-config ];

          buildInputs = [ pkgs.openssl ];
        };
      in
      {
        config = lib.mkMerge [
          (lib.mkIf config.services.activitywatch.enable {
            services.activitywatch = {
              package = pkgs.aw-server-rust;
              watchers = lib.mkIf hasWayland { awatcher.package = awatcher; };
            };
          })
        ];
      }
    )
  ];
}

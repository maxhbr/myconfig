{
  description = "my project description";

  inputs = {
    platformio-core.url = "github:platformio/platformio-core";
    platformio-core.flake = false;

    esp-idf.url = "github:espressif/esp-idf";
    esp-idf.flake = false;
  };

  outputs = { self, nixpkgs,  ... }@inputs:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      systems = [ "x86_64-linux" ];
    in
    {
      nixosModule = { config, lib, pkgs, ... }: let
        platformio-udev-rules = pkgs.writeTextFile {
          name = "platformio-udev-rules";
          text = builtins.readFile "${inputs.platformio-core}/scripts/99-platformio-udev.rules";
          destination = "/etc/udev/rules.d/99-platformio.rules";
        };
      in {
        nixpkgs.overlays = [ self.overlay ];
        home-manager.sharedModules = [{
          home.packages = (with pkgs; [
            my-west
            my-west-arm
            my-west-esp32
            my-west-init my-west-update
            platformio openocd
            minicom
          ]);
        }];
        services.udev.packages = [ platformio-udev-rules pkgs.openocd ];
      };

      overlay = import ./flake.overlay.nix inputs;

      packages = forAllSystems (system: {
        my-west = (import nixpkgs { inherit system; overlays = [ self.overlay ]; }).my-west;
        my-west-arm = (import nixpkgs { inherit system; overlays = [ self.overlay ]; }).my-west-arm;
        my-west-esp32 = (import nixpkgs { inherit system; overlays = [ self.overlay ]; }).my-west-esp32;
        my-west-init = (import nixpkgs { inherit system; overlays = [ self.overlay ]; }).my-west-init;
        my-west-update = (import nixpkgs { inherit system; overlays = [ self.overlay ]; }).my-west-update;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.my-west-arm);

      defaultApp = forAllSystems (system: {
        type = "app";
        program = "${self.defaultPackage."${system}"}/bin/mywest";
      });

      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            my-west
            my-west-arm
            my-west-esp32
            my-west-update
            my-west-init
          ];
        }
      );
    };
}

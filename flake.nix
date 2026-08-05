{
  description = "Rootless Podman + gVisor coding-agent sandboxes for NixOS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forEachSystem = f:
        nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        }));
    in
    {
      overlays.default = import ./nix/overlay.nix;

      packages = forEachSystem (pkgs: rec {
        default = agent-session;
        inherit (pkgs) agent-session agent-sandbox-image agent-sandbox-load-image;
      });

      apps = forEachSystem (pkgs: {
        default = self.apps.${pkgs.stdenv.hostPlatform.system}.agent-session;
        agent-session = {
          type = "app";
          program = "${pkgs.agent-session}/bin/agent-session";
        };
        # Build the agent image with Nix and load it into the rootless
        # Podman image store of the calling user.
        load-image = {
          type = "app";
          program = "${pkgs.agent-sandbox-load-image}/bin/agent-sandbox-load-image";
        };
      });

      nixosModules.default = import ./nixos/agent-sandboxes.nix;
      nixosModules.agent-sandboxes = self.nixosModules.default;

      devShells = forEachSystem (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            agent-session
            agent-sandbox-load-image
            gvisor
            podman
            shellcheck
          ];
        };
      });

      checks = forEachSystem (pkgs: {
        inherit (pkgs) agent-session agent-sandbox-image;
        shellcheck = pkgs.runCommand "shellcheck-agent-session"
          { nativeBuildInputs = [ pkgs.shellcheck ]; }
          ''
            shellcheck ${./bin/agent-session}
            touch $out
          '';
      });

      formatter = forEachSystem (pkgs: pkgs.nixpkgs-fmt);
    };
}

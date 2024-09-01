{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  packageToWrap = with lib;
    types.submodule {
      options = {
        pkg = mkOption { type = types.package; };

        executable = mkOption { type = types.str; };

        args = mkOption {
          type = types.str;
          default =
            "--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations";
        };

        enabled = mkOption {
          type = types.bool;
          default = true;
        };
      };
    };
  packagesToWrap = with lib; types.listOf packageToWrap;
  # packageToWrap = with lib.types; attrsOf (attrs {
  #   pkg = package;
  #   executable = str;
  #   args = mkOptionDefault types.str "--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations";
  #   enabled = mkOptionDefault types.bool true;
  # });
in {
  options.myconfig = with lib; {
    desktop.wayland = {
      wrappedElectronPackages = mkOption {
        type = packagesToWrap;
        default = [ ];
        description = lib.mdDoc ''
          Electron packages to be wrapped with `--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations`
        '';
      };
    };
  };
  config = lib.mkIf cfg.desktop.wayland.enable {
    home.packages = let
      wrapExecutable = ptw:
        pkgs.runCommand "${ptw.executable}-wl" {
          buildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          makeWrapper ${ptw.pkg}/bin/${ptw.executable} $out/bin/$(basename ${ptw.executable})-wl --add-flags "${ptw.args}"
        '';
      wrapAndAddExecutable = ptw: [ (wrapExecutable ptw) ptw.pkg ];
      maybeWrapAndAddExecutable = ptw:
        if ptw.enabled then (wrapAndAddExecutable ptw) else [ ];
    in lib.concatMap wrapAndAddExecutable
    cfg.desktop.wayland.wrappedElectronPackages;
  };
}

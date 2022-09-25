{ config, lib, pkgs, ... }:
let
  appimage-run-xdg = with pkgs;
    writeShellScriptBin "appimage-run-xdg" ''
      set -euo pipefail

      buildXDG_DATA_DIRS() {
          set -x
          requisites=$(nix-store --query --requisites --quiet --quiet ${appimage-run}/bin/appimage-run)
          gsettings_schemas_path=$(printf "$requisites" | grep gsettings-desktop-schemas | head -n1)
          gtk_path=$(printf "$requisites" | grep gtk+3 | head -n1)
          eval $(nix-store --print-env $(nix-store -qd $gsettings_schemas_path) | grep "export name")
          gsettings_schemas_name=$name
          eval $(nix-store --print-env $(nix-store -qd $gtk_path) | grep "export name")
          gtk_name=$name
          set +x
          echo $gsettings_schemas_path/share/gsettings-schemas/$gsettings_schemas_name:$gtk_path/share/gsettings-schemas/$gtk_name:$XDG_DATA_DIRS
      }
      export XDG_DATA_DIRS="$(buildXDG_DATA_DIRS)"

      ${appimage-run}/bin/appimage-run $@
    '';
in {
  config = lib.mkIf config.myconfig.desktop.enable {
    environment.systemPackages = [ appimage-run-xdg pkgs.appimage-run ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:
let
  mkscreenshot =
    with pkgs;
    writeShellScriptBin "mkscreenshot.sh" ''
      set -e

      if [[ "$1" == "--help" ]]; then
          cat <<EOF
      $0
      $0 f[ull]
      $0 w[indow]
      EOF
      fi

      output_dir="$HOME/_screenshots"
      old_dir="$output_dir/_old"
      mkdir -p "$output_dir"
      mkdir -p "$old_dir"

      if [[ "$1" == "win" ]]; then
          shift
          output="$output_dir/$(date +%Y-%m-%d_%H-%M-%S).png"
      else
          output="$output_dir/$(date +%Y-%m-%d_%H:%M:%S).png"
      fi

      echo "## clean up old screenshots ..."
      find "$output_dir" -maxdepth 1 -mtime +10 -type f -print -exec mv {} "$old_dir" \;

      echo "## take screenshot $output ..."
      if [[ "$1" == "full" || "$1" = "f"* ]]; then
         ${xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --fullscreen --open cat > "$output"
      elif [[ "$1" == "window" || "$1" = "w"* ]]; then
         ${xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --window --open cat > "$output"
      elif [[ -z "$1" ]]; then
        ${xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --region --open cat > "$output"
      else
        cat <<EOF
      $0 [f[ull]|w[indow]]
      EOF
      fi
    '';
in
{
  config = (
    lib.mkIf config.services.xserver.enable {
      home-manager.sharedModules = [ { home.packages = [ mkscreenshot ]; } ];
    }
  );
}

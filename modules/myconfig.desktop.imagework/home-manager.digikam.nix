{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.desktop.imagework.digikam;
  mydigikam = pkgs.digikam;
  mydigikam-here = (
    pkgs.writeShellScriptBin "digikam-here" ''
      here=$(pwd)
      arg="''${1:-$here}"

      # Search for existing digikam_here directory
      digikam_dir=""
      search_dir="$here"
      while [[ "$search_dir" != "/" ]]; do
        if [[ -d "$search_dir/digikam_here" ]]; then
          digikam_dir="$search_dir/digikam_here"
          break
        fi
        search_dir=$(dirname "$search_dir")
      done

      # Fallback: create new directory at ../digikam_here
      if [[ -z "$digikam_dir" ]]; then
        digikam_dir="$here/../digikam_here"
      fi

      mkdir -p "$digikam_dir"

      export XDG_CONFIG_HOME="$digikam_dir/config"
      export XDG_DATA_HOME="$digikam_dir/data"
      export XDG_CACHE_HOME="$digikam_dir/cache"

      mkdir -p "$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME"

      exec ${mydigikam}/bin/digikam "$arg"
    ''
  );
in
{
  options.myconfig.desktop.imagework.digikam.enable = lib.mkEnableOption "digikam";
  config = lib.mkIf cfg.enable {
    home.packages = [
      mydigikam
      mydigikam-here
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "digikam"
    ];
  };
}

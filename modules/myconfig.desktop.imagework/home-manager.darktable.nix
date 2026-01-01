{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.desktop.imagework.darktable;
  mydarktable = pkgs.darktable;
  mydarktable-here = (
    pkgs.writeShellScriptBin "darktable-here" ''
      here=$(pwd)
      arg="''${1:-$here}"

      # Search for existing darktable_here directory
      darktable_dir=""
      search_dir="$here"
      while [[ "$search_dir" != "/" ]]; do
        if [[ -d "$search_dir/darktable_here" ]]; then
          darktable_dir="$search_dir/darktable_here"
          break
        fi
        search_dir=$(dirname "$search_dir")
      done

      # Fallback: create new directory at ../darktable_here
      if [[ -z "$darktable_dir" ]]; then
        darktable_dir="$here/../darktable_here"
      fi

      mkdir -p "$darktable_dir"

      exec ${mydarktable}/bin/darktable \
          `#--configdir "$darktable_dir/config"` \
          --cachedir "$darktable_dir/cache" \
          --library "$darktable_dir/library.db" \
          `#--datadir "$darktable_dir/data"` \
          --tmpdir "$darktable_dir/tmp" \
          --dumpdir "$darktable_dir" \
          "$arg"
    ''
  );
in
{
  options.myconfig.desktop.imagework.darktable.enable = lib.mkEnableOption "darktable";
  config = lib.mkIf cfg.enable {
    home.packages = [
      mydarktable
      mydarktable-here
    ];
    myconfig.persistence.files = [
      ".config/darktable/darktablerc"
      ".config/darktable/shortcutsrc"
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "darktable"
    ];
  };
}

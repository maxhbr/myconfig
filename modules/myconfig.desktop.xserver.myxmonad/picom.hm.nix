{
  config,
  lib,
  pkgs,
  ...
}:
let
  restartPicom =
    with pkgs;
    writeShellScriptBin "restartPicom" ''
      ${systemd}/bin/systemctl --user restart picom.service
    '';
  stopPicom =
    with pkgs;
    writeShellScriptBin "stopPicom" ''
      ${systemd}/bin/systemctl --user stop picom.service
    '';
in
{
  home.packages = with pkgs; [
    restartPicom
    stopPicom
  ];
  home.file = {
    ".config/autorandr/postswitch.d/restartPicom".source = "${restartPicom}/bin/restartPicom";
  };
  services.picom =
    let
      excludes = [
        "window_type *= 'menu'"
        "class_g = 'firefox' && argb"
        "name ~= 'Zoom Meeting'"
        "class_g = 'zoom'"
        # "name ~= 'Firefox$'"
        # "focused = 1"
        # # "menu        = { shadow = false; };"
        # "dropdown_menu = { shadow = false; };"
        # "popup_menu    = { shadow = false; };"
        # "utility       = { shadow = false; };"
      ];
    in
    {
      enable = true;
      blur = true;
      blurExclude = excludes;
      fade = true;
      fadeExclude = excludes;
      shadow = true;
      shadowExclude = excludes;
      vSync = true;
      inactiveDim = "0.2";
      menuOpacity = "0.8";
      extraOptions = ''
        unredir-if-possible = true;
        dbe = true;
        inactive-opacity-override = false;
        # https://www.reddit.com/r/xmonad/comments/ih2scc/picom_opacity_not_working_in_xmonad/
        mark-ovredir-focused = false;
        use-ewmh-active-win = true;
      '';
    };
}

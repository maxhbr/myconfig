{ config, lib, pkgs, ... }:

{
  imports = [
    ./neomutt
    ./astroid.nix
    ./mu
    ({ config, ... }: {
      config = lib.mkIf config.programs.mbsync.enable {
        services.mbsync = {
          enable = true;
          package = config.programs.mbsync.package;
          preExec = let
            mbsync-preExec = pkgs.writeShellScriptBin "mbsync-preExec" ''
              ${pkgs.coreutils}/bin/date > "$HOME/Maildir/mbsync-start-pre-exec"
            '';
          in "${mbsync-preExec}/bin/mbsync-preExec";
          postExec = let
            mbsync-postExec = pkgs.writeShellScriptBin "mbsync-postExec" ''
              ${pkgs.coreutils}/bin/date > "$HOME/Maildir/mbsync-start-post-exec"
              ${if config.programs.notmuch.enable then
                "${pkgs.notmuch}/bin/notmuch new --no-hooks --verbose"
              else
                ""}
              ${if config.programs.mu.enable then
                "${pkgs.mu}/bin/mu index"
              else
                ""}
            '';
          in "${mbsync-postExec}/bin/mbsync-postExec";
        };
      };
    })
  ];
  config = {
    home.packages = with pkgs; [ abook extract_url urlscan ];
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.aerc = {
      enable = true;
      # see for example https://man.archlinux.org/man/aerc-config.5.en
      extraConfig = let pager = "${pkgs.bat}/bin/bat";
      in {
        general = { unsafe-accounts-conf = true; };
        ui = { mouse-enabled = true; };
        compose = {
          # editor = "${pkgs.vim}/bin/vim";
          empty-subject-warning = true;
        };
        viewer = {
          inherit pager;
          # show-headers = true;
          always-show-mime = true;
        };
        filters = {
          "text/plain" = pager;
          "text/*" =
            ''${pager} -fP --file-name="$AERC_FILENAME" --style=plain'';
          "image/*" =
            "${pkgs.catimg}/bin/catimg -w$(${pkgs.ncurses}/bin/tput cols) -";
        };
      };
    };
    programs.notmuch = {
      enable = true;
      hooks = { preNew = "mbsync --all"; };
    };
    programs.mu.enable = true;
    programs.astroid.enable = false;
    programs.neomutt.enable = true;
  };
}

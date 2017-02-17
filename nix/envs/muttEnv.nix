{pkgs}:
with pkgs; let
  name = "muttEnv";
  paths  = with pkgs; [
    mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch urlview
  ];
in buildEnv { inherit name paths; }

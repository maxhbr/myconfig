{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = lib.mkIf config.programs.gnupg.agent.enable {
    programs.gnupg.agent = {
      enableSSHSupport = !config.programs.ssh.startAgent;
      pinentryPackage = pkgs.pinentry-all;
      # pinentryPackage = if config.myconfig.desktop.enable then
      #   pkgs.pinentry-qt
      # else
      #   pkgs.pinentry-curses;
    };
    environment = {
      systemPackages = with pkgs; [ gnupg ];
    };

    # gpg-agent (in --supervised / socket-activated mode) maps any logged
    # error to a non-zero exit code: `rc = rc ? rc : log_get_errorcount(0) ? 2 : 0;`
    # (agent/gpg-agent.c:agent_exit).  A single benign `log_error` therefore
    # makes the process exit with status 2, which systemd reports as
    # `status=2/INVALIDARGUMENT` and `Failed with result 'exit-code'`.
    #
    # One common, harmless trigger is `gpg --import` of a key file that
    # contains *secret* keys which already exist in the persistent
    # `~/.gnupg/private-keys-v1.d/`.  gpg sends the `IMPORT_KEY` Assuan
    # command without `--force`; the agent returns `GPG_ERR_EEXIST` and
    # `leave_cmd()` logs `command 'IMPORT_KEY' failed: File exists`.  gpg
    # itself treats `EEXIST` as success (the key is already present), but the
    # agent has now logged an error, so its next idle shutdown exits with 2.
    #
    # Exit code 2 here does not mean the agent crashed — it means errors
    # were logged at some point during the run.  Accept it so the user
    # session does not see a spurious service failure after every key import.
    systemd.user.services.gpg-agent.serviceConfig.SuccessExitStatus = [ 2 ];
  };
}

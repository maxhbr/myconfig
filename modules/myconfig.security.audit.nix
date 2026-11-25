# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Kernel audit for privileged activity: which commands ran with effective
# uid 0, and which of those came through sudo. Opt-in, because the audit
# subsystem carries real cost (a kernel rule on every execve, plus a disk
# and journal budget) and only earns it on a host where "who ran what as
# root" is a question worth answering.
#
# Two mechanisms, deliberately both:
#
#   * sudo I/O logging (`logSudoIO`) records a replayable per-session
#     transcript: the command line AND everything it printed. It cannot be
#     evaded by execing a second binary, because sudo itself writes it.
#   * auditd execve rules (always on) record every exec with euid 0, by
#     any path — including a direct `ssh root@host`, which never touches
#     sudo. This is the part that covers deployment (`switch.sh` runs as
#     root over ssh) and any post-exploitation shell.
#
# Neither stops anything; they only record. An attacker who already has
# root can stop the daemon — `security.audit.enable = "lock"` makes that
# require a reboot, and is exposed below as an option.
#
# Shipping: audit events reach the journal through the `audisp-syslog`
# plugin (enabled by default by the nixpkgs module) at LOG_INFO, which the
# generic Alloy journal source drops at `journalMinPriority` (default:
# warning). `shipAuditLogs` therefore adds a dedicated Alloy source, the
# same way myconfig.networking.hardening does for sshd and the kernel. See
# modules/myconfig.observability/host.loki.nix for the matching recording
# rules and dashboards/security-root-activity.json for the dashboard.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.security.audit;

  clientEnabled = config.myconfig.observability.client.enable;
  alloyEnabled = clientEnabled && config.myconfig.observability.client.alloy.enable;
  shipAuditLogs = cfg.shipAuditLogs && alloyEnabled;

  # `-F auid>=1000` is what makes the rule readable: it separates "root
  # did this, traceable to a login session" from "a daemon did this".
  # auid is the *login* uid and survives sudo/su, so a root shell reached
  # from mhuber's login still carries auid=1000.
  #
  # The second rule catches root's own logins (auid=0), e.g. the
  # `root@host` path switch.sh deploys over.
  #
  # 4294967295 is the "unset" auid (-1); excluding it drops events from
  # contexts with no login attribution, which would otherwise be a torrent
  # of daemon noise.
  baseExecRules = [
    "-a always,exit -F arch=b64 -S execve -F euid=0 -F auid>=1000 -F auid!=4294967295 -k ${cfg.key}"
    "-a always,exit -F arch=b32 -S execve -F euid=0 -F auid>=1000 -F auid!=4294967295 -k ${cfg.key}"
    "-a always,exit -F arch=b64 -S execve -F euid=0 -F auid=0 -k root_login_exec"
    "-a always,exit -F arch=b32 -S execve -F euid=0 -F auid=0 -k root_login_exec"
  ];

  sudoIOConfig = lib.optionalString cfg.logSudoIO ''
    # Replayable session transcripts (sudo's own I/O log). The directory
    # is created below; `iolog_file` is relative to it.
    Defaults log_output
    Defaults iolog_dir="${cfg.iologDir}"
  '';
in
{
  options.myconfig.security.audit = {
    enable = lib.mkEnableOption "kernel audit of privileged activity";

    logSudoIO = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Turn on sudo's I/O logging: a per-session transcript with the
        command line and all of its input and output. This is the only
        mechanism that records what a command *printed*, which is usually
        what you want when reconstructing an incident.

        WARNING: the transcripts are stored unencrypted and contain
        whatever was typed or printed during the session, including any
        secret that passed through the terminal. Set `iologDir` somewhere
        with a retention policy you are comfortable with.
      '';
    };

    iologDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/log/sudo-io";
      description = "Directory holding sudo I/O session transcripts.";
    };

    key = lib.mkOption {
      type = lib.types.str;
      default = "root_exec";
      description = ''
        Audit rule key for privileged execs attributed to a login session.
        Used by the dashboard queries to select the relevant records.
      '';
    };

    lockRules = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Set `security.audit.enable = "lock"`, making the audit rules
        immutable until the next reboot. This is what turns the audit trail
        from "a record an attacker can stop" into one they have to reboot
        to escape — at the cost of not being able to change rules without
        a reboot.
      '';
    };

    shipAuditLogs = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Ship the audit journal stream to Loki, in addition to the
        priority-filtered generic source. Required for the dashboard:
        `audisp-syslog` logs audit records at LOG_INFO, which
        `journalMinPriority` (default: warning) drops.
      '';
    };

    maxLogFile = lib.mkOption {
      type = lib.types.int;
      default = 8;
      description = "Size in MiB at which auditd rotates its log.";
    };
  };

  config = lib.mkIf cfg.enable {
    security.audit.enable = if cfg.lockRules then "lock" else true;
    security.audit.rules = baseExecRules;

    security.auditd = {
      enable = true;
      settings = {
        # Keep the on-disk log small and rotate it; the durable record is
        # the journal, which Loki scrapes. Defaults (space_left 75 MiB,
        # admin_space_left 50 MiB) would sit uncomfortably close to full on
        # a small VPS root.
        max_log_file = cfg.maxLogFile;
        num_logs = 5;
        max_log_file_action = "ROTATE";
        space_left = "10%";
        space_left_action = "SYSLOG";
        admin_space_left = "5%";
        admin_space_left_action = "SYSLOG";
        # Never halt the machine because auditd ran out of room; record the
        # failure instead. The dashboards treat a silent audit stream as a
        # broken pipeline, so a loud failure is the better outcome.
        disk_full_action = "SYSLOG";
        disk_error_action = "SYSLOG";
        # Resolve uids to names in the log so the dashboard shows users
        # rather than numbers; the values are captured locally at write
        # time, before anything is shipped off the host.
        log_format = "ENRICHED";
        flush = "INCREMENTAL_ASYNC";
        freq = 50;
      };
    };

    # journald must not throttle the events we most want. The audit stream
    # is exactly the kind of bursty source the default burst limit is
    # tuned to suppress.
    services.journald.settings.Journal = lib.mkIf cfg.shipAuditLogs {
      RateLimitIntervalSec = lib.mkDefault "0";
      RateLimitBurst = lib.mkDefault 0;
    };

    # ─── sudo I/O logging ────────────────────────────────────────────────
    security.sudo.extraConfig = lib.mkIf cfg.logSudoIO sudoIOConfig;

    systemd.tmpfiles.rules = lib.mkIf cfg.logSudoIO [
      # 0700 root: the transcripts are as sensitive as the commands run.
      "d ${cfg.iologDir} 0700 root root - -"
    ];

    # ─── attack log shipping ─────────────────────────────────────────────
    # A dedicated journal source for audit records, through the documented
    # `extraConfig` escape hatch of myconfig.observability.client.alloy.
    # It reuses the `journal` relabel rules and `default` write receiver
    # the client module already defines, and labels the stream so the
    # recording rules and dashboard can select it.
    myconfig.observability.client.alloy.extraConfig = lib.mkIf shipAuditLogs ''
      loki.source.journal "audit" {
        max_age       = "${config.myconfig.observability.client.alloy.journalMaxAge}"
        matches       = "_TRANSPORT=audit"
        relabel_rules = loki.relabel.journal.rules
        forward_to    = [loki.write.default.receiver]
        labels        = {
          job          = "systemd-journal",
          log_stream   = "audit",
          host         = "${config.networking.hostName}",
        }
      }
    '';

    # The dashboard is provisioned by the observability host (see
    # modules/myconfig.observability/host.loki.nix): the audited host is not
    # the one running Grafana, so gating the dashboard on this module would
    # mean it never appears where it can be viewed.
  };
}

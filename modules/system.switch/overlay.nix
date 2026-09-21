# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# switch-to-configuration-ng decides "is this user alive?" by stat()ing
# /run/user/<uid>, claiming that the directory exists iff the user
# manager is active (BindsTo= on user@.service). That invariant does
# not hold for:
#   * the greeter user (uid 990): greetd's PAM session creates the
#     runtime dir, but user@990.service is not kept running while
#     greetd.service sits idle between sessions;
#   * agent users (uid 31000+): they have `linger = true`, so
#     user-runtime-dir@<uid>.service stays up, but user@<uid>.service
#     (which carries the session dbus-broker) can be inactive.
# The spawned per-user child then dies with
#   Failed to connect to socket /run/user/<uid>/bus: Connection refused
# and the whole switch exits 4 even though nothing that matters failed.
#
# The patch probes the session bus socket before spawning the child and
# skips users without one, matching the intent of the upstream comment.
#
# TODO: remove once upstream nixpkgs ships a fix (no upstream issue has
# been filed yet; see doc/TODOs/drop-switch-to-configuration-skip-dead-users-patch.md
# for the reproduction notes and follow-up).
_final: prev: {
  switch-to-configuration-ng = prev.switch-to-configuration-ng.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./skip-users-without-session-bus.patch
    ];
  });
}

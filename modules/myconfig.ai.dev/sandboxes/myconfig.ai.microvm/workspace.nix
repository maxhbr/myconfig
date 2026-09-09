# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — the AUTHORITY over where a task's standalone clone
# lives and how it is found again (see ./docs/workspace-layout.md).
#
# Two things are decided here, and nowhere else:
#
#   * the LAYOUT: `central` puts every clone under `<workspaceRoot>`,
#     `beside-repo` puts it next to the source repository, mirroring the
#     `<project>__worktrees` convention of workmux. Only the PARENT of the
#     per-repository group directory differs between the two; the group name
#     itself (`<repoSlug>__agent-microvm`) is computed by the launcher's
#     `repo_slug` in both cases.
#
#   * the INDEX: `<runtimeRoot>/workspace-index/<task>` is a root-owned symlink
#     to that task's clone. It is what makes a task NAME resolvable to a path
#     once clones no longer share one root, and it is the enumeration source
#     for `usage` / `dashboard` / `workspace-remove` / `recover`.
#
# The index deliberately lives under `runtimeRoot` (root-owned, never shared
# into a guest) rather than next to the clones: an untrusted agent must not be
# able to retarget an entry that `workspace-remove` later follows with `rm -rf`.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  paths = rec {
    # Storage root of the `central` layout. Under `beside-repo` nothing is
    # created here; it stays meaningful because pre-existing clones remain
    # enumerable and removable from it (see launcher.nix `each_workspace`).
    root = cfg.workspaceRoot;
    layout = cfg.workspaceLayout;
    # The reserved per-repository group-directory suffix. A directory with this
    # suffix is treated as an agent workspace group in EVERY layout, which is
    # what lets `validate_repository` refuse a clone as a source repository
    # without knowing where clones are stored.
    groupSuffix = "__agent-microvm";
    indexRoot = "${cfg.runtimeRoot}/workspace-index";
    indexEntry = task: "${indexRoot}/${task}";
  };
in
{
  config = lib.mkMerge [
    { _module.args.agentWorkspace = paths; }

    (lib.mkIf cfg.enable {
      # Root-owned and only 0755 so `usage`/`dashboard` can stat entries; it
      # holds symlinks exclusively, and only the launcher (root) writes them.
      systemd.tmpfiles.rules = [
        "d ${paths.indexRoot} 0755 root root - -"
      ];
    })
  ];
}

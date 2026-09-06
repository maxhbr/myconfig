Reviewed PR 77 at [`4d5ae8411`](https://github.com/maxhbr/myconfig/commit/4d5ae8411c25997934c290fc4e1e230284b7dd46). I would still request changes. Error propagation, direct Git-target approval, raw `nix.conf`, `/mysbx-home` replacement, and baseline destinations are improved, but three security blockers remain.

## Follow-up action items

1. **[P1] Make writable-alias analysis independent of declaration order.**
   [`check_symlinkable_dests()`](https://github.com/maxhbr/myconfig/blob/4d5ae8411c25997934c290fc4e1e230284b7dd46/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L656-L700) learns writable sources during one forward scan and only checks `src.starts_with(writable)`. It misses:

   * an RO alias declared before the RW alias of its source;
   * an RO parent containing a writable repo/RW subtree.

   For example: RW-bind `/host/tree/writable -> /w`, RO-bind `/host/tree -> /view`, then mount below `/view/writable/jump`. The payload can create `/w/jump -> /`; on the next run the nested destination escapes to the root. Precompute all persistent writable host subtrees, propagate overlaps in both directions, and add both regression cases.

2. **[P1] Reject ancestors of the Nix daemon directory too.**
   The [`network = false` guard](https://github.com/maxhbr/myconfig/blob/4d5ae8411c25997934c290fc4e1e230284b7dd46/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L239-L249) only rejects sources below `/nix/var/nix`. A read-only mount such as `/nix -> /host-nix` passes while exposing `/host-nix/var/nix/daemon-socket/socket`. Check both containment directions and apply the rule to every effective source, including the implicit repo bind.

3. **[P1] Keep trusted policy files out of writable mounts.**
   User mounts are emitted unconditionally, but nothing prevents a relocated RW mount from containing the sidecar. For repo `~/src/r`, `~/src -> /all-src` exposes `/all-src/r.mysbx/config.toml` writable. A payload can then add a broad `git-dirs` approval, rewrite its `.git` pointer, and gain RW access to another Git repository next run. This invalidates the rationale for the [sidecar widening exception](https://github.com/maxhbr/myconfig/blob/4d5ae8411c25997934c290fc4e1e230284b7dd46/modules/myconfig.ai/mysbx/docs/design/config.md#L130-L140). Protect canonical user/sidecar policy paths from all writable sources and aliases, or remove the sidecar-only widening exception.

4. **[P2] Enforce the "host home is never mounted" claim.**
   [`host_path()` accepts `~/`](https://github.com/maxhbr/myconfig/blob/4d5ae8411c25997934c290fc4e1e230284b7dd46/modules/myconfig.ai/mysbx/mysbx-rs/src/config.rs#L286-L306), so `path = "~/"; dest = "/host-home"` exposes the complete home while the report still says it is not mounted. The [Nix assertion](https://github.com/maxhbr/myconfig/blob/4d5ae8411c25997934c290fc4e1e230284b7dd46/modules/myconfig.ai/mysbx/default.nix#L227-L261) checks only the raw destination and also misses normalized spellings such as `/x/../home/user`. Reject sources equal to or containing canonical `$HOME`, and validate normalized effective destinations.

5. **[P2] Provide a recovery path after implicit initialization.**
   The primary bare/run path creates an empty sidecar without approvals, but a later explicit `mysbx init` immediately returns when [`config.toml` already exists](https://github.com/maxhbr/myconfig/blob/4d5ae8411c25997934c290fc4e1e230284b7dd46/modules/myconfig.ai/mysbx/mysbx-rs/src/lib.rs#L464-L468). Consequently, someone who first runs the primary command cannot use `init` to snapshot the discovered Git metadata. Add an explicit idempotent approval command/flag, or stop claiming this case works without manual editing.

6. **[P2] Actually activate the generated ripgrep configuration.** DONE:
   `default.nix` `baselineEnv` sets
   `RIPGREP_CONFIG_PATH = "/mysbx-home/.config/ripgrep/ripgreprc"` in the
   generated user layer's `[env]`, gated on Home Manager's own condition
   (`programs.ripgrep.enable && arguments != []`) — a variable pointing
   at a missing file is a hard `rg` failure. Pinned twice: golden argv
   test `golden_ripgrep_config_path_activation` (mount + setenv in
   section order) and execution-level
   `ripgrep_config_mount_is_activated_through_the_variable` (real bwrap:
   the payload reads the variable and the mounted file's content through
   it).

`git diff --check` passes, and the source contains 221 tests. I could not independently execute the Rust/Nix suites because this environment has neither Cargo nor Nix.

---

## Status (round 3 rework, branch `review-rework`)

All six items are done, one commit each; the full suite runs green under
real bubblewrap (`nix shell nixpkgs#bubblewrap …`), 249 tests across 6
binaries (114 lib + 0 integration + 76 bwrap + 47 cli + 13 argv + 0):

| # | item | commit |
|---|------|--------|
| 1 | order-independent writable-alias analysis | `7a04de91bf` |
| 2 | daemon-socket ancestors refused when network is denied | `068a2cdab4` |
| 3 | trusted policy files out of writable binds | `e18f29e089` |
| 4 | host home never mounted, every spelling | `fdcdade874` |
| 5 | `init --approve-git-dirs` recovery | `51b120639d` |
| 6 | ripgrep config activated through its variable | `5d9d31107f` |

Each commit went through an independent reviewer pass; their findings
(beyond what the items themselves asked for) were folded back in before
committing. Running the suite under real bwrap for the first time on this
host also exposed two test-environment assumptions, repaired in the item-6
commit: worktree fixtures now pin an empty sidecar sibling against
outer-repo debris, and no test depends on `/usr/bin/true` (absent on
plain NixOS).

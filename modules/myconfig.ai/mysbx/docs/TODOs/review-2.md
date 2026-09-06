Reviewed PR 77 at head [`b9338f638`](https://github.com/maxhbr/myconfig/commit/b9338f63829f7b550dfbf564c3cbaf737192c962). I would request changes: the “all seven findings are fixed” status is premature.

| Original finding          | Re-review                                                                             |
| ------------------------- | ------------------------------------------------------------------------------------- |
| 1. Protected destinations | Partial; lexical bypasses fixed, symlink aliases remain                               |
| 2. Network tri-state      | Fixed                                                                                 |
| 3. Bind ordering          | Partial; lexical overlaps caught, but validation panics and symlink aliases bypass it |
| 4. Worktree metadata      | Blocking security regression                                                          |
| 5. Resolver files         | Addressed at argv level                                                               |
| 6. Nix usability          | Functional path added, but it undermines network/secret isolation                     |
| 7. Dry-run argv0          | Fixed                                                                                 |

Follow-up action items:

1. **[P1] Stop trusting `.git` and `commondir` as host mount specifications.**
   [`repo_at()` accepts arbitrary canonical paths](https://github.com/maxhbr/myconfig/blob/b9338f63829f7b550dfbf564c3cbaf737192c962/modules/myconfig.ai/mysbx/mysbx-rs/src/repo.rs#L147-L158), and [`bwrap_argv()` binds every result read-write](https://github.com/maxhbr/myconfig/blob/b9338f63829f7b550dfbf564c3cbaf737192c962/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L151-L160). A repo-writable `.git` containing `gitdir: /` therefore produces `--bind / /` after the protected base mounts, exposing the host root read-write. `gitdir: $HOME` exposes the whole home.
   Safest MVP: require external Git metadata to be explicitly approved by trusted user/sidecar state rather than granting authority based on a repo-controlled file. Reject malformed, non-directory, protected, home, root, and unapproved targets. Add adversarial CLI tests for malicious `gitdir` and `commondir` values alongside legitimate worktree/submodule tests.

2. **[P1] Make destination protection robust against symlinked parent components.**
   [`check_dest()` explicitly performs only lexical normalization](https://github.com/maxhbr/myconfig/blob/b9338f63829f7b550dfbf564c3cbaf737192c962/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L331-L365). After the repo is mounted, `repo/jump -> /` makes a destination such as `repo/jump/tmp` resolve to protected `/tmp`. Bubblewrap’s [own test suite confirms that intermediate destination symlinks are followed](https://github.com/containers/bubblewrap/blob/bb3ff51ec60b40ebf0f51b33521967213f5d857e/tests/test-sandbox.py#L575-L580).
   For the MVP, reject configured destinations beneath the repo or an earlier writable bind unless there is a race-safe resolution mechanism. Host-side `canonicalize()` alone is neither an accurate model of the composed sandbox root nor race-safe.

3. **[P1] Redesign the Nix integration before exposing it unconditionally.**
   [`/nix/var/nix` and `/etc/nix/nix.conf` are always mounted](https://github.com/maxhbr/myconfig/blob/b9338f63829f7b550dfbf564c3cbaf737192c962/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L259-L272), including when `network = false`.

   * A read-only bind does not prevent connecting to the Nix daemon socket. The payload can ask the host daemon to run fixed-output derivations, which deliberately retain network access, so the report’s “network denied” claim is false.
   * The raw host `nix.conf` may contain `access-tokens`, including GitHub/GitLab tokens. Both behaviors are documented in the [Nix configuration reference](https://nix.dev/manual/nix/2.35/command-ref/conf-file).

   Do not expose the daemon under network-denied policy unless using a separately constrained service. Generate a minimal sanitized client configuration instead of mounting the host file.

4. **[P2] Return validation errors instead of panicking.**
   Protected/hidden mount failures currently call [`panic!`](https://github.com/maxhbr/myconfig/blob/b9338f63829f7b550dfbf564c3cbaf737192c962/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L177-L192), while the CLI directly invokes the builder without error handling. User-reachable invalid configuration therefore exits as a Rust panic rather than the documented exit `1` with `mysbx:`. Make the builder return `Result`, propagate it through `sandbox()`, and replace `#[should_panic]` tests with subprocess assertions.

5. **[P2] Protect the sandbox-home mount itself.**
   `/mysbx-home` is excluded wholesale from protected destinations to permit seeding descendants. That also permits `dest = "/mysbx-home"` or an ancestor such as `/mysbx`, replacing/hiding the fresh tmpfs while the report still claims it exists. Reject destinations equal to or above `/mysbx-home`; allow only strict descendants.

6. **Existing follow-up, not introduced by PR 77:** align generated grants with the new home model.
   [`baselineMounts` has no destinations](https://github.com/maxhbr/myconfig/blob/b9338f63829f7b550dfbf564c3cbaf737192c962/modules/myconfig.ai/mysbx/default.nix#L55-L68), so the configurations appear under `/home/mhuber/...`, while tools see `HOME=/mysbx-home`. They are largely undiscoverable and violate the documented “no `/home/` in argv” invariant. Give them explicit destinations below `/mysbx-home`, and decide whether user entries are grants or unconditional mounts—the current merge copies every grant into the effective mounts, so the documented “sidecar can drop grants” behavior is impossible.

`git diff --check` passes and the branch contains 171 test functions. I could not execute the Rust/Nix suite because this environment has neither Cargo nor Nix; its installed Bubblewrap also cannot initialize due the restricted `/proc` setup.

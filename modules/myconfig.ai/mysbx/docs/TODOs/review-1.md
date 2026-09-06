I found seven substantive issues in head `8688d3a28`.

> **Status (branch `review-rework`): all seven findings are fixed.** One
> commit per finding, each individually reviewed:
>
> 1. `f66e59b0d0` — `normalize()` (lexically resolves `.`/`..`) +
>    `check_dest()` rejecting dests related to a protected path in either
>    direction (equal, descendant, ancestor).
> 2. `976e4e168f` — layer `network` values became `Option<bool>`; the
>    shared-by-default `true` is applied once, after the merge, so an
>    omitted sidecar value never triggers `NetworkUpgrade`.
> 3. `6ace72107f` — `check_hidden_mounts()` in `bwrap.rs` panics when a
>    later mount's dest would hide an earlier mount (or the implicit
>    repo/git binds); argv-order semantics live with the argv builder.
> 4. `680b84d0c6` — `Repo.git_dirs` resolves `.git` files (worktrees) and
>    commondir; each git dir is bound rw right after the repo bind and
>    protected against hiding.
> 5. `774c3c4a26` — sharing the network now also binds the resolver set
>    (`/etc/hosts`, `/etc/nsswitch.conf`, `/etc/resolv.conf`, `/etc/ssl`,
>    `/run/systemd/resolve`) with `--ro-bind-try`, matching the existing
>    jail.nix `network` combinator.
> 6. `4285fc0c63` — chose "expose the narrow paths": `--ro-bind-try`
>    `/nix/var/nix` (daemon socket, store DB) and `/etc/nix/nix.conf`,
>    same binds as the base of `fns/bubblewrap-app.nix`; both are
>    protected dests.
> 7. `a743c711c9` — `--dry-run` prints `bwrap_bin` (`MYSBX_BWRAP`, read
>    before the early return) as argv[0], the first line of the argv
>    block.
>
> Test count grew from 135 to 171.

1. **[P1] Protected destinations can be bypassed.** The check is lexical: `dest = "/x/.."` passes but Bubblewrap resolves it to `/`, allowing a configured mount to replace the sandbox root. Destinations such as `/nix` also pass while hiding protected `/nix/store`. Reject `..` components and destinations that are either ancestors or descendants of protected paths. [bwrap.rs L115–138](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L115-L138)

2. **[P1] An omitted sidecar `network` value is treated as explicit `true`.** Both parsed layers default `network` to `true`, so user config `network = false` plus an empty/generated sidecar triggers `NetworkUpgrade`. In practice, a global deny makes a freshly initialized sandbox fail. Layer values need `Option<bool>` or equivalent; apply the default only after merging. [merge.rs L255–265](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/merge.rs#L255-L265)

3. **[P1] Bind ordering can silently undo read-only restrictions.** Mounts are emitted in declaration order. A user config declaring `/home/u/.ssh` read-only and then `/home/u` read-write causes the later parent bind to hide the nested read-only mount, despite the documented “deepest grant wins” rule. Reject unsafe parent-after-child overlaps or otherwise enforce effective specificity. [merge.rs L296–347](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/merge.rs#L296-L347)

4. **[P2] Git worktrees and submodules are detected but unusable.** Discovery deliberately accepts a `.git` file, but the sandbox binds only the worktree root. The `.git` file points to metadata outside that root, so commands such as `git status` fail inside the sandbox. Resolve and mount the absolute git/common directories narrowly. [repo.rs L104–117](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/repo.rs#L104-L117), [bwrap.rs L104–105](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L104-L105)

5. **[P2] `network = true` does not provide working DNS.** Sharing the network namespace is insufficient when the new root lacks `/etc/resolv.conf` and the relevant resolver target. Hostname-based `git` and `curl` calls will generally fail. The existing sandbox implementation already accounts for these resolver files. [bwrap.rs L177–194](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs#L177-L194)

6. **[P2] `nix` is shipped on `PATH` but cannot use the store.** Only `/nix/store` is bound; `/nix/var/nix`, including the daemon socket/store database, is absent. Normal multi-user `nix` operations will fail. Either expose the required narrow paths or omit `nix` until supported. [mysbx.nix L67–87](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/nix/mysbx.nix#L67-L87)

7. **[P2] `--dry-run` omits the executable it audits.** It returns after printing arguments beginning with `--clearenv`; `MYSBX_BWRAP` is read only afterward. This contradicts the packaging definition of done, which requires the wrapped store path as `argv[0]`, and prevents verification of the pinned backend. [lib.rs L223–235](https://github.com/maxhbr/myconfig/blob/8688d3a28c6dcf64f8244d28290862a891ce6f96/modules/myconfig.ai/mysbx/mysbx-rs/src/lib.rs#L223-L235)

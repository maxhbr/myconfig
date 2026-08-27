# Research notes: llama.cpp version, fork, template, and hash verification

## Version selection: b10549

### Candidates considered

| Tag | Date | Type | PR #25494? | Drift from nixpkgs b10408 |
|-----|------|------|-----------|--------------------------|
| b10056 (old pin) | 2026-07-16 | stable | No | — |
| b10408 (nixpkgs) | 2026-08-14 | stable | No | 0 (baseline) |
| **b10549 (chosen)** | **2026-08-21** | **stable** | **Yes** | **~141 commits** |
| b10642 | 2026-08-26 | prerelease | Yes | ~560 commits |
| b10646 | 2026-08-27 | prerelease | Yes | ~590 commits |

### Rationale

`b10549` is the newest **stable** (non-prerelease) tag that includes:
- PR #25494 (Vulkan: dequant q8_0 KV once in coopmat1, merged 2026-08-19)
  — a measurable Vulkan KV-cache speedup relevant to the DFlash2 candidate.
- Full Qwen3.8 / MTP / ngram-mod / draft-dflash speculative-decoding support
  (the spec types `draft-mtp`, `draft-dflash`, `ngram-mod` are all present).

It was chosen over the newer prereleases b10642/b10646 to **minimise drift**
from the nixpkgs `package.nix` this overlay builds on top of (nixpkgs ships
b10408). Lower drift = lower risk of an unbuildable combination that cannot
be caught without the gfx1151 hardware to test builds. b10549 is ~141
commits ahead of b10408 vs ~590 for b10646.

b10646 (the newest prerelease) is documented as **operator-upgradeable**:
the overlay's `version` and `hash` are localised in
`hosts/host.thing/nixpkgs.overlays.llama-cpp.nix`. An operator who has
tested a newer build on the hardware can bump it there.

### What was rechecked (not assumed)

- Upstream tag list (fetched fresh 2026-08-27); b10549 confirmed as the
  newest stable tag at task time.
- PR #25494 merge status and its inclusion in b10549 (ancestor check).
- Spec type presence (`draft-mtp`, `draft-dflash`, `ngram-mod`) in the
  b10549 source tree.
- `tools/ui/package-lock.json` byte-identical across b10408 / b10549 /
  b10646 (verified by fetching all three and diffing).

## Hash computation

All hashes were computed reproducibly via `nix build --impure --expr`
against a `fetchFromGitHub` with `lib.fakeHash`, then the resulting
error message's `got:` value was pinned. This mirrors how nixpkgs
maintainers compute SRI hashes.

### Source hash (b10549)

```bash
nix build --impure --no-link --expr '
  (import <nixpkgs> {}).fetchFromGitHub {
    owner = "ggml-org"; repo = "llama.cpp"; tag = "b10549";
    hash = lib.fakeHash; leaveDotGit = true;
    postFetch = "git -C $out rev-parse --short HEAD > $out/COMMIT; find $out -name .git -print0 | xargs -0 rm -rf";
  }'
# got: sha256-ULVNojWLWvNCCqggfrK5+hZqmscbOaqoTa7n5r/jDm8=
```

### npmDepsHash (b10549)

```bash
nix build --impure --no-link --expr '
  let pkgs = import <nixpkgs> {}; in
  pkgs.fetchNpmDeps {
    src = pkgs.fetchFromGitHub { owner = "ggml-org"; repo = "llama.cpp";
      tag = "b10549"; hash = "sha256-ULVNojWLWvNCCqggfrK5+hZqmscbOaqoTa7n5r/jDm8=";
      leaveDotGit = true; postFetch = "git -C $out rev-parse --short HEAD > $out/COMMIT; find $out -name .git -print0 | xargs -0 rm -rf"; };
    hash = pkgs.lib.fakeHash; npmRoot = "tools/ui"; }
'
# got: sha256-2Q7XhaLAArmviOLdQsNbYTfdyDE5pW9lR26cRHEVl9k=
```

This npmDepsHash is **identical to nixpkgs b10408** because
`tools/ui/package-lock.json` is byte-identical between the two tags.

## Fork: Nathanw1014 strix-halo-vulkan

### Commit verification

- Repository: `Nathanw1014/llama.cpp`
- Branch: `strix-halo-vulkan` (a named branch, NOT a moving ref)
- Commit: `0eb528051a56f34567312ce63ab4e14a3fc71d89`
- Verified: the commit is on the `strix-halo-vulkan` branch (fetched
  the branch ref and confirmed the commit is an ancestor).

### Fork hash

- Source: `sha256-2PG8G3P4q+S4TUH4Te/tOStrHqrDycpxJZeiBc+89kI=`
  (computed via the same fetchFromGitHub method, verified by fetching)
- npmDepsHash: `sha256-2Q7XhaLAArmviOLdQsNbYTfdyDE5pW9lR26cRHEVl9k=`
  (the fork's `tools/ui/package-lock.json` is byte-identical to b10549's,
  verified by fetching both and diffing — so the same npmDepsHash applies)

### Packaging

The fork is built as `llama-cpp-strix-halo` via
`final.llama-cpp-vulkan.overrideAttrs { src = <fork>; }` — it inherits
the b10549 build infrastructure (cmake flags, deps) + vulkanSupport and
only swaps the source. It is a **separate derivation** so the upstream
Vulkan/ROCm/CUDA backends stay on b10549. Only the DFlash2 candidate
references it via `serverPackage`.

## Template: sharp.jinja

### Provenance

- Upstream: `PieBru/Qwen-3.8-27B_Strix-Halo_gfx1151` repo, `models/sharp.jinja`
- License: MIT (Copyright (c) 2026 PieBru)
- Template version (line 1): `qwen3.8-froggeric-v22.3`
- SHA-256: `6e1439c913ad7df4a966493ad70de7e7fc5a548d41bbe417c1571f766603629b`
  (verified by `sha256sum` of the vendored copy)
- Vendored at: `modules/myconfig.ai/myconfig.ai.llama-cpp/templates/sharp.jinja`
- Provenance file: `modules/.../templates/sharp.jinja.provenance`

### Why

The Qwen3.8-27B GGUF embedded chat template drops trailing
`assistant(tool_calls)` in the auto-prefill/continuation path (upstream
llama.cpp issue #27588). `sharp.jinja` is a reviewed, tested replacement
that handles OpenAI-compatible tool calls, `enable_thinking` /
`preserve_thinking` / `preserve_reasoning` kwargs, and reasoning-effort
steering. Loaded with `--jinja --chat-template <store-path>`.

## Model SHA-256 (LFS oids)

Verified via the HuggingFace `/api/.../tree/main?blobs=true` endpoint
(2026-08-27). These are logged in the server startup banner for
provenance but NOT verified at runtime (hashing a multi-GB file at
startup would add minutes of I/O).

| Model | Repo | SHA-256 (LFS oid) |
|-------|------|-------------------|
| Q6_K_XL (target, DFlash2) | unsloth/Qwen3.8-27B-GGUF | `701d8fa9ed214ab21bfc130cd2a7df19ca89bbef7713e2dfb19f3c63696aa917` |
| Q4_K_XL (target, ROCm) | unsloth/Qwen3.8-27B-GGUF | `3f227079003add2511437e5b1e94812e363385225bf6a9b47b0054a72bc8b01e` |
| Q8_0 (base, MTP) | unsloth/Qwen3.8-27B-GGUF | `a680f44a06920e5d689774823782006aa3acc8db95750323373b24139b67e348` |
| mtp-Q8_0 (draft, ROCm) | ggml-org/Qwen3.8-27B-GGUF | `cbf60a0c48b431bb61f1d49b8948dc88ac29c398d6dbdbbb2e6e89ef77eacc9a` |
| DFlash2-Q8_0 (draft, DFlash2) | z-lab/Qwen3.8-27B-DFlash2-GGUF | `c18e800daedc59ca68fd13b6a856d795746af6d399a9279ac6a277d1d422f87e` |

**Note:** the DFlash2-Q8_0 oid `c18e800d...` differs from the
`7f1c9a31...` value in PieBru's `models.ini`. The HuggingFace API
value (`c18e800d...`) is authoritative; PieBru's value is stale. The
correct oid is pinned in the candidate's `sha256` field.

## Version-drift assertion

`modules/.../version-check.nix` exposes
`myconfig.ai.llama-cpp.backendVersions` (per-backend version/srcRev/
srcTag/outPath) and asserts `base = vulkan = rocm`. A host overlay that
pins only one of them would fail this assertion at eval time, making
the drift immediately detectable. (cuda is omitted — it tracks base by
construction via `override`, and forcing it would pull cudaPackages
into AMD-only evals.)

Verified: `nix eval .#nixosConfigurations.test-thing.config.myconfig.ai.llama-cpp.backendVersions`
shows base=vulkan=rocm=10549.

# Drop the fastapi source patch for the pinned litellm

## Context

`modules/myconfig.ai/services.litellm.nix` sets
`services.litellm.package` via
`pkgs.python3Packages.litellm.overridePythonAttrs`. The litellm 1.97.0
source (as shipped by the pinned `nixpkgs` input) imports
`fastapi.dependencies.utils.get_flat_dependant`, which fastapi
>= 0.140.7 removed (the pinned input ships 0.141.x), so the proxy crashes
at startup in
`litellm/proxy/management_endpoints/management_v1/common.py`
(`_declared_query_params`) with that `ImportError`.

The `postPatch` appended there therefore rewrites the pinned source to the
upstream form (`get_flat_params` + `ParamTypes.query`, BerriAI/litellm
commit f9b86b253a3f, "fix(proxy): restore query-param validation under
fastapi>=0.140.7"). The step is self-invalidating: its `grep` finds no file
while a litellm source without the old import is bundled, so once nixpkgs
bundles a release containing the fix the step silently does nothing.
`pythonImportsCheck` imports `litellm` and the formerly crashing module, so
the build fails again if the source-vs-fastapi incompatibility regresses.

Introducing commits:
- `7ba7985bd7` — the earlier local `get_flat_dependant` shim
  (*replaced by this patch; do not reuse*);
- the commit that introduced the `postPatch` above: child of
  `7ba7985bd7`, titled "litellm: patch pinned 1.97.0 for fastapi >=
  0.140.7"; `git log -S get_flat_dependant --
  modules/myconfig.ai/services.litellm.nix` lists the exact hash.

## When to act

When the `nixpkgs` flake input pins a litellm that contains
BerriAI/litellm f9b86b253a3f (any release newer than 1.97.0, or a
backport).

## Steps

1. **Verify** after the input bump: `nix build --impure --print-out-paths
   .#nixosConfigurations.test-thing.config.services.litellm.package`
   builds, the build log still runs the two `pythonImportsCheck` imports,
   and `grep -Rn get_flat_dependant <the printed path>/lib/` finds nothing
   while `grep -Rn get_flat_params <the printed path>/lib/python3*/site-packages/litellm/proxy/management_endpoints/management_v1/common.py`
   matches (proof the bundled source now carries the fix and our `postPatch`
   did nothing).
2. **Remove** in `modules/myconfig.ai/services.litellm.nix` the `postPatch`
   append and the `pythonImportsCheck` entry in the `overridePythonAttrs`
   addition (keep the `dependencies` additions: litellm's `proxy` /
   `extra_proxy` / `proxy-runtime` further deps plus `expression` and
   `prometheus`), and trim the adjacent comments to the dependency-closure
   note only. Run `./nixfmtall.sh`.
3. **Verify:** rebuild with the same command as in step 1 and restart the
   service on a deployed host that serves it (e.g. `hosts/host.thing`:
   `systemctl restart litellm.service`; the unit listens on
   `config.services.litellm.host` on port 4000 — on `thing` that evals to
   0.0.0.0:4000; check the peer-reachable URL
   `http://<thing-wg-ip>:4000/health/readiness`).
4. **Delete this file.**

## Verified on introduction

- `nix build --impure --print-out-paths
  .#nixosConfigurations.test-thing.config.services.litellm.package`
  succeeds (the `test-f13` host shares the same package drvPath); the
  built `common.py` carries `get_flat_params` / `ParamTypes.query` with no
  `get_flat_dependant` left, and the launcher env bundles the packaged
  `expression` 5.6.0 dependency.
- The proxy starts under fastapi 0.141.1: `/health/readiness` returns
  `{"status": "healthy"}`, and
  `GET /management/v1/spend_logs/end_users?dummy_undeclared=1` returns 400
  `unknown-query-parameter` with
  `allowed: ["filter[startTime][gte]", "filter[startTime][lte]", "page",
  "page_size", "q"]` — proof the rewritten query-param filter runs
  end-to-end on the new FastAPI.

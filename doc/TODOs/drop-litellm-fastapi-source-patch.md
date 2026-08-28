# Drop the local fastapi compatibility patch for the pinned litellm

## Context

`services.litellm` builds litellm 1.97.0 from the pinned `nixpkgs` input
with a `postPatch` that applies upstream BerriAI/litellm commit
`f9b86b253a3f` ("fix(proxy): restore query-param validation under
fastapi>=0.140.7") to the 1.97.0 source. litellm releases up to and
including 1.97.0 import `fastapi.dependencies.utils.get_flat_dependant`,
which fastapi 0.140.7+ (the current nixpkgs-pinned 0.141.x) removed; no
released litellm contains the upstream fix yet, so the patch is the only
way to make 1.97.0 start under the pinned fastapi. The patched code
(`get_flat_params` + `ParamTypes`) works with fastapi 0.139.0 and 0.141.x
alike. When the litellm source itself gains the fix, the `postPatch` step
detects it (its `grep` matches nothing) and skips itself silently.

- **Code:** the `overrideAttrs` block in `modules/myconfig.ai/services.litellm.nix`
  (`package = ...`: the `postPatch` attribute and the
  `litellm.proxy.management_endpoints.management_v1.common` entry in
  `pythonImportsCheck`). Find the introducing commit with
  `git log -S get_flat_dependant -- modules/myconfig.ai/services.litellm.nix`.

## When to act

Once the `nixpkgs` flake input pins a litellm revision that contains
commit `f9b86b253a3f` (i.e. any litellm release newer than 1.97.0 or a
direct backport).

## Steps

1. **Verify:** after the next input bump, the crash site in the by-name
   litellm definition is gone —
   `nix build --impure --print-out-paths .#nixosConfigurations.test-thing.config.services.litellm.package`
   and `grep -R get_flat_dependant <the printed path>` yields nothing
   (the new litellm stores the upstream-fixed source), *and* the service
   import check still passes.
2. **Remove the patch bits:** in `modules/myconfig.ai/services.litellm.nix`,
   delete the `overrideAttrs` block (keep the `overridePythonAttrs`
   dependency additions), drop the added `pythonImportsCheck` entry,
   re-run `./nixfmtall.sh`.
3. **Verify:** rebuild and start the service on `hosts/host.thing`
   (`systemctl restart litellm && journalctl -u litellm | grep -iE 'error|import'`).
   It should listen on 127.0.0.1:4000 (host overrides it to 0.0.0.0 for wg0).
4. **Delete this file.**

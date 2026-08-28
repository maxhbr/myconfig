# Drop the litellm `get_flat_dependant` fastapi compat shim

## What

`modules/myconfig.ai/services.litellm.nix` patches the litellm source
(`fastapiCompatPatch` in the `let` block, applied via `postPatch` in
`services.litellm.package`).

The patch removes

```python
from fastapi.dependencies.utils import get_flat_dependant
```

from
`litellm/proxy/management_endpoints/management_v1/common.py`
and appends a local re-implementation of `get_flat_dependant` at the end of
that module.

## Why

litellm 1.97.0 imports `get_flat_dependant` from
`fastapi.dependencies.utils`. FastAPI removed that helper in 0.141.0 (only
`get_flat_params` remains). The `nixpkgs` input ships fastapi 0.141.1, so
without the patch `litellm.service` fails at startup with:

```
ImportError: cannot import name 'get_flat_dependant' from 'fastapi.dependencies.utils'
```

The workaround was added in the commit that also removed the now-obsolete
locally built `expression` package (nixpkgs packages
`python3Packages.expression` 5.6.0 and litellm's `proxy` extra already
depends on it).

## When to remove

Once `nixpkgs` ships a litellm version that no longer imports
`get_flat_dependant` (i.e. is compatible with fastapi >= 0.141).

Check:

```bash
p=$(./get_input.sh nixpkgs)
grep -n 'version =' $p/pkgs/development/python-modules/litellm/default.nix
nix build --no-link --print-out-paths .#nixosConfigurations.test-f13.pkgs.python3Packages.litellm
grep -rn get_flat_dependant <result>/lib/python3*/site-packages/litellm/ --include=*.py
```

If the grep finds nothing:

1. Delete the `fastapiCompatPatch` let-binding and its `postPatch` use.
2. Replace the whole `package = pkgs.python3Packages.toPythonApplication (...)`
   expression with plain `pkgs.litellm` (the nixpkgs by-name wrapper already
   adds the `proxy`, `extra_proxy` and `proxy-runtime` extras).
3. Delete this file.

Verify with:

```bash
p=$(nix build --no-link --print-out-paths .#nixosConfigurations.test-f13.config.services.litellm.package)
$p/bin/litellm --port 4999 --host 127.0.0.1 &
curl -s http://127.0.0.1:4999/health/readiness
```

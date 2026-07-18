# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# input-remapper overlay: (a) add missing `packaging` runtime dependency and
# (b) patch `configs/data.py` so the top-level `import pkg_resources`
# (removed in setuptools 82) becomes a lazy import inside the existing
# try/except fallback in `_try_python_package_location`.
#
# Upstream `inputremapper/configs/migrations.py` does
#   from packaging import version
# and `configs/data.py` does
#   import pkg_resources
# but the nixpkgs `package.nix` `dependencies` list omits `packaging`,
# and setuptools 82 (Python 3.14) no longer ships `pkg_resources`.
#
# TODO: remove once nixpkgs adds `packaging` to input-remapper deps and
# patches the `pkg_resources` import (or upstream input-remapper releases
# a version that drops `pkg_resources`).
_final: prev: {
  input-remapper = prev.input-remapper.overridePythonAttrs (old: {
    dependencies = (old.dependencies or [ ]) ++ [
      prev.python3Packages.packaging
    ];
    patches = (old.patches or [ ]) ++ [
      ./pkg-resources.patch
    ];
  });
}

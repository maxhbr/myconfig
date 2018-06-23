# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super: {
  pass = super.pass.overrideDerivation ( drv: {
    # should work for 1.7.2
    patches = drv.patches ++ [ ./patches/pass_-_copy_by_default.diff ];
  });
}

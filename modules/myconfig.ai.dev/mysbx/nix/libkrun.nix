# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ libkrun }:
(libkrun.override { withBlk = true; }).overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ [ ./patches/libkrun-readdirplus-lookups.patch ];
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    cargo test --offline --locked -p krun-devices --features blk readdirplus -- --test-threads=1
    runHook postCheck
  '';
})

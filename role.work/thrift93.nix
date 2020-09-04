# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  version = "0.9.3";
  name = "thrift-${version}";
in {
  thrift93 = super.thrift.overrideAttrs (oldAttrs: {
    inherit name version;
    src = self.fetchurl {
      url = "http://archive.apache.org/dist/thrift/${version}/${name}.tar.gz";
      sha256 = "17lnchan9q3qdg222rgjjai6819j9k755s239phdv6n0183hlx5h";
    };
  });
}

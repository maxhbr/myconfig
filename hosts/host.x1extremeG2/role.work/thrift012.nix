# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  version = "0.12.0";
  name = "thrift-${version}";
in {
  thrift012 = super.thrift.overrideAttrs (oldAttrs: {
    inherit name version;
    src = self.fetchurl {
      url = "http://archive.apache.org/dist/thrift/${version}/${name}.tar.gz";
      sha256 = "0a04v7dgm1qzgii7v0sisnljhxc9xpq2vxkka60scrdp6aahjdn3";
    };
  });
}

# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  version = "0.11.0";
  pname = "thrift";
in {
  thrift011 = super.thrift.overrideAttrs ( oldAttrs: {
    inherit pname version;
    src = self.fetchurl {
      url = "http://archive.apache.org/dist/thrift/${version}/${pname}-${version}.tar.gz";
      sha256 = "1hk0zb9289gf920rdl0clmwqx6kvygz92nj01lqrhd2arfv3ibf4";
    };
  });
}

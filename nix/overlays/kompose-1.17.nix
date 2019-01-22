# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  version = "1.17.0";
  name = "kompose-${version}";
in {
  kompose117 = super.kompose.overrideAttrs ( oldAttrs: {
    inherit name version;
    src = self.fetchFromGitHub {
      rev = "v${version}";
      owner = "kubernetes";
      repo = "kompose";
      sha256 = "0ngil7l36s46vhnpaa6i6yma6gbf20i058vjl6v44n8ahxrb49w6";
    };
  });
}

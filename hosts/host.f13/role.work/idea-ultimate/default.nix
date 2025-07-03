# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  jsonFile = ./idea-ultimate.json;
  json = builtins.fromJSON (builtins.readFile jsonFile);
  pkgsVersion = "2018.1.4"; # super.lib.getVersion super.idea.idea-ultimate;
  overlayIsNewer = super.lib.versionOlder pkgsVersion json.version;
in
if overlayIsNewer then
  {
    idea-ultimate = super.idea.idea-ultimate.overrideAttrs (oldAttrs: {
      inherit (json) version;
      name = "idea-ultimate-${json.version}";
      src = super.fetchurl {
        url = "https://download.jetbrains.com/idea/ideaIU-${json.version}.tar.gz";
        inherit (json) sha256;
      };
    });
  }
else
  {
    idea-ultimate = super.idea.idea-ultimate;
  }

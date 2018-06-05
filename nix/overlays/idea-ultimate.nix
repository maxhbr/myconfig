# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  version = "2018.1.4";
  sha256 = "1122dhk7ad3a8iw7fs55kg7cjj657hcxbk2ykhd3x0h2759y25pv";
  pkgsVersion = "2018.1.4"; # super.lib.getVersion super.idea.idea-ultimate;
  overlayIsNewer =  super.lib.versionOlder pkgsVersion version;
in if overlayIsNewer
   then {
     idea-ultimate = super.idea.idea-ultimate.overrideAttrs ( oldAttrs: {
       inherit version;
       name = "idea-ultimate-${version}";
       src = super.fetchurl {
         url = "https://download.jetbrains.com/idea/ideaIU-${version}.tar.gz";
         inherit sha256;
       };
     });
   } else {
     idea-ultimate = super.idea.idea-ultimate;
   }

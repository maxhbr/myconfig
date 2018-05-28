# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super:
let
  name = "idea-ultimate-${version}";
  version = "2018.1.4";
  sha256 = "1122dhk7ad3a8iw7fs55kg7cjj657hcxbk2ykhd3x0h2759y25pv";
  oldVersion = "2017.2.5"; # super.lib.getVersion super.idea.idea-ultimate;
  overlayIsNewer =  super.lib.versionOlder oldVersion version;
in if overlayIsNewer
   then {
     idea-ultimate = super.idea.idea-ultimate.overrideAttrs ( oldAttrs: {
       inherit name version;
       src = super.fetchurl {
         url = "https://download.jetbrains.com/idea/ideaIU-${version}.tar.gz";
         inherit sha256;
       };
     });
   } else {
    idea-ultimate = super.idea.idea-ultimate;
   }

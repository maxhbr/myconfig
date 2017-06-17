{stdenv, pkgs, fetchurl}:
pkgs.idea.idea-ultimate.overrideDerivation (super: rec {
  name = "idea-ultimate-${version}";
  version = "2017.1.4";
  src = fetchurl {
    url = "https://download.jetbrains.com/idea/ideaIU-${version}.tar.gz";
    sha256 = "0a93fba480pvdh6x263fm7rb9w728smgx65nbvkkbdqngjmnanyx";
  };
})

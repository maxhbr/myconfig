self: super: with self; {
  premake5 = callPackage ../pkgs/premake5 {};
  otfcc = callPackage ../pkgs/otfcc { inherit premake5; };
  iosevka = callPackage ../pkgs/iosevka { inherit otfcc; };
  imposevka = callPackage ../pkgs/iosevka/imposevka.nix { inherit otfcc; };
}

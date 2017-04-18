{ self, fetchurl, fetchgit ? null, lib }:

{
  by-spec."amdefine".">=0.0.4" =
    self.by-version."amdefine"."1.0.1";
  by-version."amdefine"."1.0.1" = self.buildNodePackage {
    name = "amdefine-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/amdefine/-/amdefine-1.0.1.tgz";
      name = "amdefine-1.0.1.tgz";
      sha1 = "4a5282ac164729e93619bcfd3ad151f817ce91f5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."ansi-regex"."^2.0.0" =
    self.by-version."ansi-regex"."2.1.1";
  by-version."ansi-regex"."2.1.1" = self.buildNodePackage {
    name = "ansi-regex-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-2.1.1.tgz";
      name = "ansi-regex-2.1.1.tgz";
      sha1 = "c3b33ab5ee360d86e0e628f0468ae7ef27d654df";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."bezier-js"."^2.0.0" =
    self.by-version."bezier-js"."2.2.3";
  by-version."bezier-js"."2.2.3" = self.buildNodePackage {
    name = "bezier-js-2.2.3";
    version = "2.2.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/bezier-js/-/bezier-js-2.2.3.tgz";
      name = "bezier-js-2.2.3.tgz";
      sha1 = "c5574116a4a3564a71538d73e0b0d515da8ddec5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "bezier-js" = self.by-version."bezier-js"."2.2.3";
  by-spec."bezier-js"."^2.2.1" =
    self.by-version."bezier-js"."2.2.3";
  by-spec."builtin-modules"."^1.0.0" =
    self.by-version."builtin-modules"."1.1.1";
  by-version."builtin-modules"."1.1.1" = self.buildNodePackage {
    name = "builtin-modules-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/builtin-modules/-/builtin-modules-1.1.1.tgz";
      name = "builtin-modules-1.1.1.tgz";
      sha1 = "270f076c5a72c02f5b65a47df94c5fe3a278892f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."camelcase"."^3.0.0" =
    self.by-version."camelcase"."3.0.0";
  by-version."camelcase"."3.0.0" = self.buildNodePackage {
    name = "camelcase-3.0.0";
    version = "3.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/camelcase/-/camelcase-3.0.0.tgz";
      name = "camelcase-3.0.0.tgz";
      sha1 = "32fc4b9fcdaf845fcdf7e73bb97cac2261f0ab0a";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."caryll-shapeops"."^0.2.0" =
    self.by-version."caryll-shapeops"."0.2.1";
  by-version."caryll-shapeops"."0.2.1" = self.buildNodePackage {
    name = "caryll-shapeops-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/caryll-shapeops/-/caryll-shapeops-0.2.1.tgz";
      name = "caryll-shapeops-0.2.1.tgz";
      sha1 = "b8404ca50e6900c07abc9357b9bb0085b4046bcb";
    };
    deps = {
      "bezier-js-2.2.3" = self.by-version."bezier-js"."2.2.3";
      "clipper-lib-1.0.0" = self.by-version."clipper-lib"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "caryll-shapeops" = self.by-version."caryll-shapeops"."0.2.1";
  by-spec."clipper-lib"."^1.0.0" =
    self.by-version."clipper-lib"."1.0.0";
  by-version."clipper-lib"."1.0.0" = self.buildNodePackage {
    name = "clipper-lib-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/clipper-lib/-/clipper-lib-1.0.0.tgz";
      name = "clipper-lib-1.0.0.tgz";
      sha1 = "e902ceb26e28ee5cfa6014abdf790a51efff4ecb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cliui"."^3.2.0" =
    self.by-version."cliui"."3.2.0";
  by-version."cliui"."3.2.0" = self.buildNodePackage {
    name = "cliui-3.2.0";
    version = "3.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cliui/-/cliui-3.2.0.tgz";
      name = "cliui-3.2.0.tgz";
      sha1 = "120601537a916d29940f934da3b48d585a39213d";
    };
    deps = {
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
      "wrap-ansi-2.1.0" = self.by-version."wrap-ansi"."2.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."code-point-at"."^1.0.0" =
    self.by-version."code-point-at"."1.1.0";
  by-version."code-point-at"."1.1.0" = self.buildNodePackage {
    name = "code-point-at-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/code-point-at/-/code-point-at-1.1.0.tgz";
      name = "code-point-at-1.1.0.tgz";
      sha1 = "0d070b4d043a5bea33a2f1a40e2edb3d9a4ccf77";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."cubic2quad"."^1.0.0" =
    self.by-version."cubic2quad"."1.1.0";
  by-version."cubic2quad"."1.1.0" = self.buildNodePackage {
    name = "cubic2quad-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/cubic2quad/-/cubic2quad-1.1.0.tgz";
      name = "cubic2quad-1.1.0.tgz";
      sha1 = "78abec71b1b0244fdca863910ced8e07446a90e7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "cubic2quad" = self.by-version."cubic2quad"."1.1.0";
  by-spec."cubic2quad"."^1.1.0" =
    self.by-version."cubic2quad"."1.1.0";
  by-spec."decamelize"."^1.1.1" =
    self.by-version."decamelize"."1.2.0";
  by-version."decamelize"."1.2.0" = self.buildNodePackage {
    name = "decamelize-1.2.0";
    version = "1.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/decamelize/-/decamelize-1.2.0.tgz";
      name = "decamelize-1.2.0.tgz";
      sha1 = "f6534d15148269b20352e7bee26f501f9a191290";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."deep-is"."~0.1.2" =
    self.by-version."deep-is"."0.1.3";
  by-version."deep-is"."0.1.3" = self.buildNodePackage {
    name = "deep-is-0.1.3";
    version = "0.1.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/deep-is/-/deep-is-0.1.3.tgz";
      name = "deep-is-0.1.3.tgz";
      sha1 = "b369d6fb5dbc13eecf524f91b070feedc357cf34";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."deep-is"."~0.1.3" =
    self.by-version."deep-is"."0.1.3";
  by-spec."error-ex"."^1.2.0" =
    self.by-version."error-ex"."1.3.1";
  by-version."error-ex"."1.3.1" = self.buildNodePackage {
    name = "error-ex-1.3.1";
    version = "1.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/error-ex/-/error-ex-1.3.1.tgz";
      name = "error-ex-1.3.1.tgz";
      sha1 = "f855a86ce61adc4e8621c3cda21e7a7612c3a8dc";
    };
    deps = {
      "is-arrayish-0.2.1" = self.by-version."is-arrayish"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."escodegen".">=1.1.0" =
    self.by-version."escodegen"."1.8.1";
  by-version."escodegen"."1.8.1" = self.buildNodePackage {
    name = "escodegen-1.8.1";
    version = "1.8.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/escodegen/-/escodegen-1.8.1.tgz";
      name = "escodegen-1.8.1.tgz";
      sha1 = "5a5b53af4693110bebb0867aa3430dd3b70a1018";
    };
    deps = {
      "estraverse-1.9.3" = self.by-version."estraverse"."1.9.3";
      "esutils-2.0.2" = self.by-version."esutils"."2.0.2";
      "esprima-2.7.3" = self.by-version."esprima"."2.7.3";
      "optionator-0.8.2" = self.by-version."optionator"."0.8.2";
    };
    optionalDependencies = {
      "source-map-0.2.0" = self.by-version."source-map"."0.2.0";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."escodegen"."~1.3.2" =
    self.by-version."escodegen"."1.3.3";
  by-version."escodegen"."1.3.3" = self.buildNodePackage {
    name = "escodegen-1.3.3";
    version = "1.3.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/escodegen/-/escodegen-1.3.3.tgz";
      name = "escodegen-1.3.3.tgz";
      sha1 = "f024016f5a88e046fd12005055e939802e6c5f23";
    };
    deps = {
      "esutils-1.0.0" = self.by-version."esutils"."1.0.0";
      "estraverse-1.5.1" = self.by-version."estraverse"."1.5.1";
      "esprima-1.1.1" = self.by-version."esprima"."1.1.1";
    };
    optionalDependencies = {
      "source-map-0.1.43" = self.by-version."source-map"."0.1.43";
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."escope"."~1.0.1" =
    self.by-version."escope"."1.0.3";
  by-version."escope"."1.0.3" = self.buildNodePackage {
    name = "escope-1.0.3";
    version = "1.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/escope/-/escope-1.0.3.tgz";
      name = "escope-1.0.3.tgz";
      sha1 = "759dce8496c4248fec2d0caaf4108bcf3f1a7f5d";
    };
    deps = {
      "estraverse-2.0.0" = self.by-version."estraverse"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esmangle".">=1.0.1" =
    self.by-version."esmangle"."1.0.1";
  by-version."esmangle"."1.0.1" = self.buildNodePackage {
    name = "esmangle-1.0.1";
    version = "1.0.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/esmangle/-/esmangle-1.0.1.tgz";
      name = "esmangle-1.0.1.tgz";
      sha1 = "d9bb37b8f8eafbf4e6d4ed6b7aa2956abbd3c4c2";
    };
    deps = {
      "esutils-1.0.0" = self.by-version."esutils"."1.0.0";
      "esshorten-1.1.1" = self.by-version."esshorten"."1.1.1";
      "estraverse-1.5.1" = self.by-version."estraverse"."1.5.1";
      "escope-1.0.3" = self.by-version."escope"."1.0.3";
      "source-map-0.1.43" = self.by-version."source-map"."0.1.43";
      "escodegen-1.3.3" = self.by-version."escodegen"."1.3.3";
      "optionator-0.3.0" = self.by-version."optionator"."0.3.0";
      "esprima-1.1.1" = self.by-version."esprima"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esprima"."^2.7.1" =
    self.by-version."esprima"."2.7.3";
  by-version."esprima"."2.7.3" = self.buildNodePackage {
    name = "esprima-2.7.3";
    version = "2.7.3";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/esprima/-/esprima-2.7.3.tgz";
      name = "esprima-2.7.3.tgz";
      sha1 = "96e3b70d5779f6ad49cd032673d1c312767ba581";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esprima"."~1.1.1" =
    self.by-version."esprima"."1.1.1";
  by-version."esprima"."1.1.1" = self.buildNodePackage {
    name = "esprima-1.1.1";
    version = "1.1.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/esprima/-/esprima-1.1.1.tgz";
      name = "esprima-1.1.1.tgz";
      sha1 = "5b6f1547f4d102e670e140c509be6771d6aeb549";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esshorten"."~1.1.0" =
    self.by-version."esshorten"."1.1.1";
  by-version."esshorten"."1.1.1" = self.buildNodePackage {
    name = "esshorten-1.1.1";
    version = "1.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/esshorten/-/esshorten-1.1.1.tgz";
      name = "esshorten-1.1.1.tgz";
      sha1 = "174f96b7cc267e46872d814e7db7c290bdff61a9";
    };
    deps = {
      "estraverse-4.1.1" = self.by-version."estraverse"."4.1.1";
      "escope-1.0.3" = self.by-version."escope"."1.0.3";
      "esutils-2.0.2" = self.by-version."esutils"."2.0.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."estraverse"."^1.9.1" =
    self.by-version."estraverse"."1.9.3";
  by-version."estraverse"."1.9.3" = self.buildNodePackage {
    name = "estraverse-1.9.3";
    version = "1.9.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/estraverse/-/estraverse-1.9.3.tgz";
      name = "estraverse-1.9.3.tgz";
      sha1 = "af67f2dc922582415950926091a4005d29c9bb44";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."estraverse"."^2.0.0" =
    self.by-version."estraverse"."2.0.0";
  by-version."estraverse"."2.0.0" = self.buildNodePackage {
    name = "estraverse-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/estraverse/-/estraverse-2.0.0.tgz";
      name = "estraverse-2.0.0.tgz";
      sha1 = "5ae46963243600206674ccb24a09e16674fcdca1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."estraverse"."~1.5.0" =
    self.by-version."estraverse"."1.5.1";
  by-version."estraverse"."1.5.1" = self.buildNodePackage {
    name = "estraverse-1.5.1";
    version = "1.5.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/estraverse/-/estraverse-1.5.1.tgz";
      name = "estraverse-1.5.1.tgz";
      sha1 = "867a3e8e58a9f84618afb6c2ddbcd916b7cbaf71";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."estraverse"."~4.1.1" =
    self.by-version."estraverse"."4.1.1";
  by-version."estraverse"."4.1.1" = self.buildNodePackage {
    name = "estraverse-4.1.1";
    version = "4.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/estraverse/-/estraverse-4.1.1.tgz";
      name = "estraverse-4.1.1.tgz";
      sha1 = "f6caca728933a850ef90661d0e17982ba47111a2";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esutils"."^2.0.2" =
    self.by-version."esutils"."2.0.2";
  by-version."esutils"."2.0.2" = self.buildNodePackage {
    name = "esutils-2.0.2";
    version = "2.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/esutils/-/esutils-2.0.2.tgz";
      name = "esutils-2.0.2.tgz";
      sha1 = "0abf4f1caa5bcb1f7a9d8acc6dea4faaa04bac9b";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esutils"."~ 1.0.0" =
    self.by-version."esutils"."1.0.0";
  by-version."esutils"."1.0.0" = self.buildNodePackage {
    name = "esutils-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/esutils/-/esutils-1.0.0.tgz";
      name = "esutils-1.0.0.tgz";
      sha1 = "8151d358e20c8acc7fb745e7472c0025fe496570";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."esutils"."~1.0.0" =
    self.by-version."esutils"."1.0.0";
  by-spec."esutils"."~2.0.2" =
    self.by-version."esutils"."2.0.2";
  by-spec."fast-levenshtein"."~1.0.0" =
    self.by-version."fast-levenshtein"."1.0.7";
  by-version."fast-levenshtein"."1.0.7" = self.buildNodePackage {
    name = "fast-levenshtein-1.0.7";
    version = "1.0.7";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-1.0.7.tgz";
      name = "fast-levenshtein-1.0.7.tgz";
      sha1 = "0178dcdee023b92905193af0959e8a7639cfdcb9";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."fast-levenshtein"."~2.0.4" =
    self.by-version."fast-levenshtein"."2.0.6";
  by-version."fast-levenshtein"."2.0.6" = self.buildNodePackage {
    name = "fast-levenshtein-2.0.6";
    version = "2.0.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz";
      name = "fast-levenshtein-2.0.6.tgz";
      sha1 = "3d8a5c66883a16a30ca8643e851f19baa7797917";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."find-up"."^1.0.0" =
    self.by-version."find-up"."1.1.2";
  by-version."find-up"."1.1.2" = self.buildNodePackage {
    name = "find-up-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/find-up/-/find-up-1.1.2.tgz";
      name = "find-up-1.1.2.tgz";
      sha1 = "6b2e9822b1a2ce0a60ab64d610eccad53cb24d0f";
    };
    deps = {
      "path-exists-2.1.0" = self.by-version."path-exists"."2.1.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."get-caller-file"."^1.0.1" =
    self.by-version."get-caller-file"."1.0.2";
  by-version."get-caller-file"."1.0.2" = self.buildNodePackage {
    name = "get-caller-file-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/get-caller-file/-/get-caller-file-1.0.2.tgz";
      name = "get-caller-file-1.0.2.tgz";
      sha1 = "f702e63127e7e231c160a80c1554acb70d5047e5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."graceful-fs"."^4.1.2" =
    self.by-version."graceful-fs"."4.1.11";
  by-version."graceful-fs"."4.1.11" = self.buildNodePackage {
    name = "graceful-fs-4.1.11";
    version = "4.1.11";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.1.11.tgz";
      name = "graceful-fs-4.1.11.tgz";
      sha1 = "0e8bdfe4d1ddb8854d64e04ea7c00e2a026e5658";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."hosted-git-info"."^2.1.4" =
    self.by-version."hosted-git-info"."2.4.2";
  by-version."hosted-git-info"."2.4.2" = self.buildNodePackage {
    name = "hosted-git-info-2.4.2";
    version = "2.4.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-2.4.2.tgz";
      name = "hosted-git-info-2.4.2.tgz";
      sha1 = "0076b9f46a270506ddbaaea56496897460612a67";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."invert-kv"."^1.0.0" =
    self.by-version."invert-kv"."1.0.0";
  by-version."invert-kv"."1.0.0" = self.buildNodePackage {
    name = "invert-kv-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/invert-kv/-/invert-kv-1.0.0.tgz";
      name = "invert-kv-1.0.0.tgz";
      sha1 = "104a8e4aaca6d3d8cd157a8ef8bfab2d7a3ffdb6";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-arrayish"."^0.2.1" =
    self.by-version."is-arrayish"."0.2.1";
  by-version."is-arrayish"."0.2.1" = self.buildNodePackage {
    name = "is-arrayish-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz";
      name = "is-arrayish-0.2.1.tgz";
      sha1 = "77c99840527aa8ecb1a8ba697b80645a7a926a9d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-builtin-module"."^1.0.0" =
    self.by-version."is-builtin-module"."1.0.0";
  by-version."is-builtin-module"."1.0.0" = self.buildNodePackage {
    name = "is-builtin-module-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-builtin-module/-/is-builtin-module-1.0.0.tgz";
      name = "is-builtin-module-1.0.0.tgz";
      sha1 = "540572d34f7ac3119f8f76c30cbc1b1e037affbe";
    };
    deps = {
      "builtin-modules-1.1.1" = self.by-version."builtin-modules"."1.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-fullwidth-code-point"."^1.0.0" =
    self.by-version."is-fullwidth-code-point"."1.0.0";
  by-version."is-fullwidth-code-point"."1.0.0" = self.buildNodePackage {
    name = "is-fullwidth-code-point-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-1.0.0.tgz";
      name = "is-fullwidth-code-point-1.0.0.tgz";
      sha1 = "ef9e31386f031a7f0d643af82fde50c457ef00cb";
    };
    deps = {
      "number-is-nan-1.0.1" = self.by-version."number-is-nan"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."is-utf8"."^0.2.0" =
    self.by-version."is-utf8"."0.2.1";
  by-version."is-utf8"."0.2.1" = self.buildNodePackage {
    name = "is-utf8-0.2.1";
    version = "0.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/is-utf8/-/is-utf8-0.2.1.tgz";
      name = "is-utf8-0.2.1.tgz";
      sha1 = "4b0da1442104d1b336340e80797e865cf39f7d72";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."lcid"."^1.0.0" =
    self.by-version."lcid"."1.0.0";
  by-version."lcid"."1.0.0" = self.buildNodePackage {
    name = "lcid-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/lcid/-/lcid-1.0.0.tgz";
      name = "lcid-1.0.0.tgz";
      sha1 = "308accafa0bc483a3867b4b6f2b9506251d1b835";
    };
    deps = {
      "invert-kv-1.0.0" = self.by-version."invert-kv"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."levn"."~0.2.4" =
    self.by-version."levn"."0.2.5";
  by-version."levn"."0.2.5" = self.buildNodePackage {
    name = "levn-0.2.5";
    version = "0.2.5";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/levn/-/levn-0.2.5.tgz";
      name = "levn-0.2.5.tgz";
      sha1 = "ba8d339d0ca4a610e3a3f145b9caf48807155054";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
      "type-check-0.3.2" = self.by-version."type-check"."0.3.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."levn"."~0.3.0" =
    self.by-version."levn"."0.3.0";
  by-version."levn"."0.3.0" = self.buildNodePackage {
    name = "levn-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz";
      name = "levn-0.3.0.tgz";
      sha1 = "3b09924edf9f083c0490fdd4c0bc4421e04764ee";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
      "type-check-0.3.2" = self.by-version."type-check"."0.3.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."libspiro-js".">=0.3.0" =
    self.by-version."libspiro-js"."0.3.1";
  by-version."libspiro-js"."0.3.1" = self.buildNodePackage {
    name = "libspiro-js-0.3.1";
    version = "0.3.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/libspiro-js/-/libspiro-js-0.3.1.tgz";
      name = "libspiro-js-0.3.1.tgz";
      sha1 = "86652b0009a6d84ea79a5320bdca5f00612ee439";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "libspiro-js" = self.by-version."libspiro-js"."0.3.1";
  by-spec."load-json-file"."^1.0.0" =
    self.by-version."load-json-file"."1.1.0";
  by-version."load-json-file"."1.1.0" = self.buildNodePackage {
    name = "load-json-file-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/load-json-file/-/load-json-file-1.1.0.tgz";
      name = "load-json-file-1.1.0.tgz";
      sha1 = "956905708d58b4bab4c2261b04f59f31c99374c0";
    };
    deps = {
      "graceful-fs-4.1.11" = self.by-version."graceful-fs"."4.1.11";
      "parse-json-2.2.0" = self.by-version."parse-json"."2.2.0";
      "pify-2.3.0" = self.by-version."pify"."2.3.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
      "strip-bom-2.0.0" = self.by-version."strip-bom"."2.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."normalize-package-data"."^2.3.2" =
    self.by-version."normalize-package-data"."2.3.6";
  by-version."normalize-package-data"."2.3.6" = self.buildNodePackage {
    name = "normalize-package-data-2.3.6";
    version = "2.3.6";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/normalize-package-data/-/normalize-package-data-2.3.6.tgz";
      name = "normalize-package-data-2.3.6.tgz";
      sha1 = "498fa420c96401f787402ba21e600def9f981fff";
    };
    deps = {
      "hosted-git-info-2.4.2" = self.by-version."hosted-git-info"."2.4.2";
      "is-builtin-module-1.0.0" = self.by-version."is-builtin-module"."1.0.0";
      "semver-5.3.0" = self.by-version."semver"."5.3.0";
      "validate-npm-package-license-3.0.1" = self.by-version."validate-npm-package-license"."3.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."number-is-nan"."^1.0.0" =
    self.by-version."number-is-nan"."1.0.1";
  by-version."number-is-nan"."1.0.1" = self.buildNodePackage {
    name = "number-is-nan-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/number-is-nan/-/number-is-nan-1.0.1.tgz";
      name = "number-is-nan-1.0.1.tgz";
      sha1 = "097b602b53422a522c1afb8790318336941a011d";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."object-assign"."^4.1.1" =
    self.by-version."object-assign"."4.1.1";
  by-version."object-assign"."4.1.1" = self.buildNodePackage {
    name = "object-assign-4.1.1";
    version = "4.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz";
      name = "object-assign-4.1.1.tgz";
      sha1 = "2109adc7965887cfc05cbbd442cac8bfbb360863";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "object-assign" = self.by-version."object-assign"."4.1.1";
  by-spec."optionator"."^0.8.1" =
    self.by-version."optionator"."0.8.2";
  by-version."optionator"."0.8.2" = self.buildNodePackage {
    name = "optionator-0.8.2";
    version = "0.8.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/optionator/-/optionator-0.8.2.tgz";
      name = "optionator-0.8.2.tgz";
      sha1 = "364c5e409d3f4d6301d6c0b4c05bba50180aeb64";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
      "deep-is-0.1.3" = self.by-version."deep-is"."0.1.3";
      "wordwrap-1.0.0" = self.by-version."wordwrap"."1.0.0";
      "type-check-0.3.2" = self.by-version."type-check"."0.3.2";
      "levn-0.3.0" = self.by-version."levn"."0.3.0";
      "fast-levenshtein-2.0.6" = self.by-version."fast-levenshtein"."2.0.6";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."optionator"."~0.3.0" =
    self.by-version."optionator"."0.3.0";
  by-version."optionator"."0.3.0" = self.buildNodePackage {
    name = "optionator-0.3.0";
    version = "0.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/optionator/-/optionator-0.3.0.tgz";
      name = "optionator-0.3.0.tgz";
      sha1 = "9715a8b5f5e7586cff06c8249e039cd7364d3f54";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
      "deep-is-0.1.3" = self.by-version."deep-is"."0.1.3";
      "wordwrap-0.0.3" = self.by-version."wordwrap"."0.0.3";
      "type-check-0.3.2" = self.by-version."type-check"."0.3.2";
      "levn-0.2.5" = self.by-version."levn"."0.2.5";
      "fast-levenshtein-1.0.7" = self.by-version."fast-levenshtein"."1.0.7";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."os-locale"."^1.4.0" =
    self.by-version."os-locale"."1.4.0";
  by-version."os-locale"."1.4.0" = self.buildNodePackage {
    name = "os-locale-1.4.0";
    version = "1.4.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/os-locale/-/os-locale-1.4.0.tgz";
      name = "os-locale-1.4.0.tgz";
      sha1 = "20f9f17ae29ed345e8bde583b13d2009803c14d9";
    };
    deps = {
      "lcid-1.0.0" = self.by-version."lcid"."1.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."otfcc-c2q"."^0.5.0" =
    self.by-version."otfcc-c2q"."0.5.0";
  by-version."otfcc-c2q"."0.5.0" = self.buildNodePackage {
    name = "otfcc-c2q-0.5.0";
    version = "0.5.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/otfcc-c2q/-/otfcc-c2q-0.5.0.tgz";
      name = "otfcc-c2q-0.5.0.tgz";
      sha1 = "ff8961236a36ee99244b07ec16531e941f0afde1";
    };
    deps = {
      "cubic2quad-1.1.0" = self.by-version."cubic2quad"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "otfcc-c2q" = self.by-version."otfcc-c2q"."0.5.0";
  by-spec."parse-json"."^2.2.0" =
    self.by-version."parse-json"."2.2.0";
  by-version."parse-json"."2.2.0" = self.buildNodePackage {
    name = "parse-json-2.2.0";
    version = "2.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/parse-json/-/parse-json-2.2.0.tgz";
      name = "parse-json-2.2.0.tgz";
      sha1 = "f480f40434ef80741f8469099f8dea18f55a4dc9";
    };
    deps = {
      "error-ex-1.3.1" = self.by-version."error-ex"."1.3.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."patel".">=0.32.0" =
    self.by-version."patel"."0.32.1";
  by-version."patel"."0.32.1" = self.buildNodePackage {
    name = "patel-0.32.1";
    version = "0.32.1";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/patel/-/patel-0.32.1.tgz";
      name = "patel-0.32.1.tgz";
      sha1 = "9e70b6b23fd7a9c82c579ed591b8afa5217e6212";
    };
    deps = {
      "patrisika-0.21.0" = self.by-version."patrisika"."0.21.0";
      "escodegen-1.8.1" = self.by-version."escodegen"."1.8.1";
      "esmangle-1.0.1" = self.by-version."esmangle"."1.0.1";
      "yargs-6.6.0" = self.by-version."yargs"."6.6.0";
      "resumer-0.0.0" = self.by-version."resumer"."0.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "patel" = self.by-version."patel"."0.32.1";
  by-spec."path-exists"."^2.0.0" =
    self.by-version."path-exists"."2.1.0";
  by-version."path-exists"."2.1.0" = self.buildNodePackage {
    name = "path-exists-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-exists/-/path-exists-2.1.0.tgz";
      name = "path-exists-2.1.0.tgz";
      sha1 = "0feb6c64f0fc518d9a754dd5efb62c7022761f4b";
    };
    deps = {
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."path-type"."^1.0.0" =
    self.by-version."path-type"."1.1.0";
  by-version."path-type"."1.1.0" = self.buildNodePackage {
    name = "path-type-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/path-type/-/path-type-1.1.0.tgz";
      name = "path-type-1.1.0.tgz";
      sha1 = "59c44f7ee491da704da415da5a4070ba4f8fe441";
    };
    deps = {
      "graceful-fs-4.1.11" = self.by-version."graceful-fs"."4.1.11";
      "pify-2.3.0" = self.by-version."pify"."2.3.0";
      "pinkie-promise-2.0.1" = self.by-version."pinkie-promise"."2.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."patrisika".">=0.21.0" =
    self.by-version."patrisika"."0.21.0";
  by-version."patrisika"."0.21.0" = self.buildNodePackage {
    name = "patrisika-0.21.0";
    version = "0.21.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/patrisika/-/patrisika-0.21.0.tgz";
      name = "patrisika-0.21.0.tgz";
      sha1 = "b82c27f7d4041522dc5c31b43b7c6efb19e47482";
    };
    deps = {
      "patrisika-scopes-0.11.0" = self.by-version."patrisika-scopes"."0.11.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."patrisika-scopes".">=0.11.0" =
    self.by-version."patrisika-scopes"."0.11.0";
  by-version."patrisika-scopes"."0.11.0" = self.buildNodePackage {
    name = "patrisika-scopes-0.11.0";
    version = "0.11.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/patrisika-scopes/-/patrisika-scopes-0.11.0.tgz";
      name = "patrisika-scopes-0.11.0.tgz";
      sha1 = "052a9d764a165e0b34cc35b6562a88242036b403";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pify"."^2.0.0" =
    self.by-version."pify"."2.3.0";
  by-version."pify"."2.3.0" = self.buildNodePackage {
    name = "pify-2.3.0";
    version = "2.3.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pify/-/pify-2.3.0.tgz";
      name = "pify-2.3.0.tgz";
      sha1 = "ed141a6ac043a849ea588498e7dca8b15330e90c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pinkie"."^2.0.0" =
    self.by-version."pinkie"."2.0.4";
  by-version."pinkie"."2.0.4" = self.buildNodePackage {
    name = "pinkie-2.0.4";
    version = "2.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pinkie/-/pinkie-2.0.4.tgz";
      name = "pinkie-2.0.4.tgz";
      sha1 = "72556b80cfa0d48a974e80e77248e80ed4f7f870";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."pinkie-promise"."^2.0.0" =
    self.by-version."pinkie-promise"."2.0.1";
  by-version."pinkie-promise"."2.0.1" = self.buildNodePackage {
    name = "pinkie-promise-2.0.1";
    version = "2.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/pinkie-promise/-/pinkie-promise-2.0.1.tgz";
      name = "pinkie-promise-2.0.1.tgz";
      sha1 = "2135d6dfa7a358c069ac9b178776288228450ffa";
    };
    deps = {
      "pinkie-2.0.4" = self.by-version."pinkie"."2.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."prelude-ls"."~1.1.0" =
    self.by-version."prelude-ls"."1.1.2";
  by-version."prelude-ls"."1.1.2" = self.buildNodePackage {
    name = "prelude-ls-1.1.2";
    version = "1.1.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz";
      name = "prelude-ls-1.1.2.tgz";
      sha1 = "21932a549f5e52ffd9a827f570e04be62a97da54";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."prelude-ls"."~1.1.2" =
    self.by-version."prelude-ls"."1.1.2";
  by-spec."read-pkg"."^1.0.0" =
    self.by-version."read-pkg"."1.1.0";
  by-version."read-pkg"."1.1.0" = self.buildNodePackage {
    name = "read-pkg-1.1.0";
    version = "1.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/read-pkg/-/read-pkg-1.1.0.tgz";
      name = "read-pkg-1.1.0.tgz";
      sha1 = "f5ffaa5ecd29cb31c0474bca7d756b6bb29e3f28";
    };
    deps = {
      "load-json-file-1.1.0" = self.by-version."load-json-file"."1.1.0";
      "normalize-package-data-2.3.6" = self.by-version."normalize-package-data"."2.3.6";
      "path-type-1.1.0" = self.by-version."path-type"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."read-pkg-up"."^1.0.1" =
    self.by-version."read-pkg-up"."1.0.1";
  by-version."read-pkg-up"."1.0.1" = self.buildNodePackage {
    name = "read-pkg-up-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/read-pkg-up/-/read-pkg-up-1.0.1.tgz";
      name = "read-pkg-up-1.0.1.tgz";
      sha1 = "9d63c13276c065918d57f002a57f40a1b643fb02";
    };
    deps = {
      "find-up-1.1.2" = self.by-version."find-up"."1.1.2";
      "read-pkg-1.1.0" = self.by-version."read-pkg"."1.1.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."require-directory"."^2.1.1" =
    self.by-version."require-directory"."2.1.1";
  by-version."require-directory"."2.1.1" = self.buildNodePackage {
    name = "require-directory-2.1.1";
    version = "2.1.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz";
      name = "require-directory-2.1.1.tgz";
      sha1 = "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."require-main-filename"."^1.0.1" =
    self.by-version."require-main-filename"."1.0.1";
  by-version."require-main-filename"."1.0.1" = self.buildNodePackage {
    name = "require-main-filename-1.0.1";
    version = "1.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/require-main-filename/-/require-main-filename-1.0.1.tgz";
      name = "require-main-filename-1.0.1.tgz";
      sha1 = "97f717b69d48784f5f526a6c5aa8ffdda055a4d1";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."resumer"."*" =
    self.by-version."resumer"."0.0.0";
  by-version."resumer"."0.0.0" = self.buildNodePackage {
    name = "resumer-0.0.0";
    version = "0.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/resumer/-/resumer-0.0.0.tgz";
      name = "resumer-0.0.0.tgz";
      sha1 = "f1e8f461e4064ba39e82af3cdc2a8c893d076759";
    };
    deps = {
      "through-2.3.8" = self.by-version."through"."2.3.8";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."semver"."2 || 3 || 4 || 5" =
    self.by-version."semver"."5.3.0";
  by-version."semver"."5.3.0" = self.buildNodePackage {
    name = "semver-5.3.0";
    version = "5.3.0";
    bin = true;
    src = fetchurl {
      url = "https://registry.npmjs.org/semver/-/semver-5.3.0.tgz";
      name = "semver-5.3.0.tgz";
      sha1 = "9b2ce5d3de02d17c6012ad326aa6b4d0cf54f94f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."set-blocking"."^2.0.0" =
    self.by-version."set-blocking"."2.0.0";
  by-version."set-blocking"."2.0.0" = self.buildNodePackage {
    name = "set-blocking-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz";
      name = "set-blocking-2.0.0.tgz";
      sha1 = "045f9782d011ae9a6803ddd382b24392b3d890f7";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."~0.1.33" =
    self.by-version."source-map"."0.1.43";
  by-version."source-map"."0.1.43" = self.buildNodePackage {
    name = "source-map-0.1.43";
    version = "0.1.43";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.1.43.tgz";
      name = "source-map-0.1.43.tgz";
      sha1 = "c24bc146ca517c1471f5dacbe2571b2b7f9e3346";
    };
    deps = {
      "amdefine-1.0.1" = self.by-version."amdefine"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."source-map"."~0.2.0" =
    self.by-version."source-map"."0.2.0";
  by-version."source-map"."0.2.0" = self.buildNodePackage {
    name = "source-map-0.2.0";
    version = "0.2.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/source-map/-/source-map-0.2.0.tgz";
      name = "source-map-0.2.0.tgz";
      sha1 = "dab73fbcfc2ba819b4de03bd6f6eaa48164b3f9d";
    };
    deps = {
      "amdefine-1.0.1" = self.by-version."amdefine"."1.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-correct"."~1.0.0" =
    self.by-version."spdx-correct"."1.0.2";
  by-version."spdx-correct"."1.0.2" = self.buildNodePackage {
    name = "spdx-correct-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-correct/-/spdx-correct-1.0.2.tgz";
      name = "spdx-correct-1.0.2.tgz";
      sha1 = "4b3073d933ff51f3912f03ac5519498a4150db40";
    };
    deps = {
      "spdx-license-ids-1.2.2" = self.by-version."spdx-license-ids"."1.2.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-expression-parse"."~1.0.0" =
    self.by-version."spdx-expression-parse"."1.0.4";
  by-version."spdx-expression-parse"."1.0.4" = self.buildNodePackage {
    name = "spdx-expression-parse-1.0.4";
    version = "1.0.4";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-expression-parse/-/spdx-expression-parse-1.0.4.tgz";
      name = "spdx-expression-parse-1.0.4.tgz";
      sha1 = "9bdf2f20e1f40ed447fbe273266191fced51626c";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."spdx-license-ids"."^1.0.2" =
    self.by-version."spdx-license-ids"."1.2.2";
  by-version."spdx-license-ids"."1.2.2" = self.buildNodePackage {
    name = "spdx-license-ids-1.2.2";
    version = "1.2.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/spdx-license-ids/-/spdx-license-ids-1.2.2.tgz";
      name = "spdx-license-ids-1.2.2.tgz";
      sha1 = "c9df7a3424594ade6bd11900d596696dc06bac57";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."string-width"."^1.0.1" =
    self.by-version."string-width"."1.0.2";
  by-version."string-width"."1.0.2" = self.buildNodePackage {
    name = "string-width-1.0.2";
    version = "1.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/string-width/-/string-width-1.0.2.tgz";
      name = "string-width-1.0.2.tgz";
      sha1 = "118bdf5b8cdc51a2a7e70d211e07e2b0b9b107d3";
    };
    deps = {
      "code-point-at-1.1.0" = self.by-version."code-point-at"."1.1.0";
      "is-fullwidth-code-point-1.0.0" = self.by-version."is-fullwidth-code-point"."1.0.0";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."string-width"."^1.0.2" =
    self.by-version."string-width"."1.0.2";
  by-spec."strip-ansi"."^3.0.0" =
    self.by-version."strip-ansi"."3.0.1";
  by-version."strip-ansi"."3.0.1" = self.buildNodePackage {
    name = "strip-ansi-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-3.0.1.tgz";
      name = "strip-ansi-3.0.1.tgz";
      sha1 = "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf";
    };
    deps = {
      "ansi-regex-2.1.1" = self.by-version."ansi-regex"."2.1.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."strip-ansi"."^3.0.1" =
    self.by-version."strip-ansi"."3.0.1";
  by-spec."strip-bom"."^2.0.0" =
    self.by-version."strip-bom"."2.0.0";
  by-version."strip-bom"."2.0.0" = self.buildNodePackage {
    name = "strip-bom-2.0.0";
    version = "2.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/strip-bom/-/strip-bom-2.0.0.tgz";
      name = "strip-bom-2.0.0.tgz";
      sha1 = "6219a85616520491f35788bdbf1447a99c7e6b0e";
    };
    deps = {
      "is-utf8-0.2.1" = self.by-version."is-utf8"."0.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."through"."~2.3.4" =
    self.by-version."through"."2.3.8";
  by-version."through"."2.3.8" = self.buildNodePackage {
    name = "through-2.3.8";
    version = "2.3.8";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/through/-/through-2.3.8.tgz";
      name = "through-2.3.8.tgz";
      sha1 = "0dd4c9ffaabc357960b1b724115d7e0e86a2e1f5";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."toml".">=2.3.0" =
    self.by-version."toml"."2.3.2";
  by-version."toml"."2.3.2" = self.buildNodePackage {
    name = "toml-2.3.2";
    version = "2.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/toml/-/toml-2.3.2.tgz";
      name = "toml-2.3.2.tgz";
      sha1 = "5eded5ca42887924949fd06eb0e955656001e834";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "toml" = self.by-version."toml"."2.3.2";
  by-spec."topsort"."0.0.2" =
    self.by-version."topsort"."0.0.2";
  by-version."topsort"."0.0.2" = self.buildNodePackage {
    name = "topsort-0.0.2";
    version = "0.0.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/topsort/-/topsort-0.0.2.tgz";
      name = "topsort-0.0.2.tgz";
      sha1 = "2e5e0ee8a1439417f101d5b9b2d035e620263321";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "topsort" = self.by-version."topsort"."0.0.2";
  by-spec."type-check"."~0.3.1" =
    self.by-version."type-check"."0.3.2";
  by-version."type-check"."0.3.2" = self.buildNodePackage {
    name = "type-check-0.3.2";
    version = "0.3.2";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz";
      name = "type-check-0.3.2.tgz";
      sha1 = "5884cab512cf1d355e3fb784f30804b2b520db72";
    };
    deps = {
      "prelude-ls-1.1.2" = self.by-version."prelude-ls"."1.1.2";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."type-check"."~0.3.2" =
    self.by-version."type-check"."0.3.2";
  by-spec."unorm".">=1.4.1" =
    self.by-version."unorm"."1.4.1";
  by-version."unorm"."1.4.1" = self.buildNodePackage {
    name = "unorm-1.4.1";
    version = "1.4.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/unorm/-/unorm-1.4.1.tgz";
      name = "unorm-1.4.1.tgz";
      sha1 = "364200d5f13646ca8bcd44490271335614792300";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "unorm" = self.by-version."unorm"."1.4.1";
  by-spec."validate-npm-package-license"."^3.0.1" =
    self.by-version."validate-npm-package-license"."3.0.1";
  by-version."validate-npm-package-license"."3.0.1" = self.buildNodePackage {
    name = "validate-npm-package-license-3.0.1";
    version = "3.0.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/validate-npm-package-license/-/validate-npm-package-license-3.0.1.tgz";
      name = "validate-npm-package-license-3.0.1.tgz";
      sha1 = "2804babe712ad3379459acfbe24746ab2c303fbc";
    };
    deps = {
      "spdx-correct-1.0.2" = self.by-version."spdx-correct"."1.0.2";
      "spdx-expression-parse-1.0.4" = self.by-version."spdx-expression-parse"."1.0.4";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."which-module"."^1.0.0" =
    self.by-version."which-module"."1.0.0";
  by-version."which-module"."1.0.0" = self.buildNodePackage {
    name = "which-module-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/which-module/-/which-module-1.0.0.tgz";
      name = "which-module-1.0.0.tgz";
      sha1 = "bba63ca861948994ff307736089e3b96026c2a4f";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wordwrap"."~0.0.2" =
    self.by-version."wordwrap"."0.0.3";
  by-version."wordwrap"."0.0.3" = self.buildNodePackage {
    name = "wordwrap-0.0.3";
    version = "0.0.3";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wordwrap/-/wordwrap-0.0.3.tgz";
      name = "wordwrap-0.0.3.tgz";
      sha1 = "a3d5da6cd5c0bc0008d37234bbaf1bed63059107";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wordwrap"."~1.0.0" =
    self.by-version."wordwrap"."1.0.0";
  by-version."wordwrap"."1.0.0" = self.buildNodePackage {
    name = "wordwrap-1.0.0";
    version = "1.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wordwrap/-/wordwrap-1.0.0.tgz";
      name = "wordwrap-1.0.0.tgz";
      sha1 = "27584810891456a4171c8d0226441ade90cbcaeb";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."wrap-ansi"."^2.0.0" =
    self.by-version."wrap-ansi"."2.1.0";
  by-version."wrap-ansi"."2.1.0" = self.buildNodePackage {
    name = "wrap-ansi-2.1.0";
    version = "2.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-2.1.0.tgz";
      name = "wrap-ansi-2.1.0.tgz";
      sha1 = "d8fc3d284dd05794fe84973caecdd1cf824fdd85";
    };
    deps = {
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "strip-ansi-3.0.1" = self.by-version."strip-ansi"."3.0.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."y18n"."^3.2.1" =
    self.by-version."y18n"."3.2.1";
  by-version."y18n"."3.2.1" = self.buildNodePackage {
    name = "y18n-3.2.1";
    version = "3.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/y18n/-/y18n-3.2.1.tgz";
      name = "y18n-3.2.1.tgz";
      sha1 = "6d15fba884c08679c0d77e88e7759e811e07fa41";
    };
    deps = {
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs"."*" =
    self.by-version."yargs"."7.1.0";
  by-version."yargs"."7.1.0" = self.buildNodePackage {
    name = "yargs-7.1.0";
    version = "7.1.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-7.1.0.tgz";
      name = "yargs-7.1.0.tgz";
      sha1 = "6ba318eb16961727f5d284f8ea003e8d6154d0c8";
    };
    deps = {
      "camelcase-3.0.0" = self.by-version."camelcase"."3.0.0";
      "cliui-3.2.0" = self.by-version."cliui"."3.2.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "get-caller-file-1.0.2" = self.by-version."get-caller-file"."1.0.2";
      "os-locale-1.4.0" = self.by-version."os-locale"."1.4.0";
      "read-pkg-up-1.0.1" = self.by-version."read-pkg-up"."1.0.1";
      "require-directory-2.1.1" = self.by-version."require-directory"."2.1.1";
      "require-main-filename-1.0.1" = self.by-version."require-main-filename"."1.0.1";
      "set-blocking-2.0.0" = self.by-version."set-blocking"."2.0.0";
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "which-module-1.0.0" = self.by-version."which-module"."1.0.0";
      "y18n-3.2.1" = self.by-version."y18n"."3.2.1";
      "yargs-parser-5.0.0" = self.by-version."yargs-parser"."5.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  "yargs" = self.by-version."yargs"."7.1.0";
  by-spec."yargs"."^6.6.0" =
    self.by-version."yargs"."6.6.0";
  by-version."yargs"."6.6.0" = self.buildNodePackage {
    name = "yargs-6.6.0";
    version = "6.6.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs/-/yargs-6.6.0.tgz";
      name = "yargs-6.6.0.tgz";
      sha1 = "782ec21ef403345f830a808ca3d513af56065208";
    };
    deps = {
      "camelcase-3.0.0" = self.by-version."camelcase"."3.0.0";
      "cliui-3.2.0" = self.by-version."cliui"."3.2.0";
      "decamelize-1.2.0" = self.by-version."decamelize"."1.2.0";
      "get-caller-file-1.0.2" = self.by-version."get-caller-file"."1.0.2";
      "os-locale-1.4.0" = self.by-version."os-locale"."1.4.0";
      "read-pkg-up-1.0.1" = self.by-version."read-pkg-up"."1.0.1";
      "require-directory-2.1.1" = self.by-version."require-directory"."2.1.1";
      "require-main-filename-1.0.1" = self.by-version."require-main-filename"."1.0.1";
      "set-blocking-2.0.0" = self.by-version."set-blocking"."2.0.0";
      "string-width-1.0.2" = self.by-version."string-width"."1.0.2";
      "which-module-1.0.0" = self.by-version."which-module"."1.0.0";
      "y18n-3.2.1" = self.by-version."y18n"."3.2.1";
      "yargs-parser-4.2.1" = self.by-version."yargs-parser"."4.2.1";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs-parser"."^4.2.0" =
    self.by-version."yargs-parser"."4.2.1";
  by-version."yargs-parser"."4.2.1" = self.buildNodePackage {
    name = "yargs-parser-4.2.1";
    version = "4.2.1";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-4.2.1.tgz";
      name = "yargs-parser-4.2.1.tgz";
      sha1 = "29cceac0dc4f03c6c87b4a9f217dd18c9f74871c";
    };
    deps = {
      "camelcase-3.0.0" = self.by-version."camelcase"."3.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
  by-spec."yargs-parser"."^5.0.0" =
    self.by-version."yargs-parser"."5.0.0";
  by-version."yargs-parser"."5.0.0" = self.buildNodePackage {
    name = "yargs-parser-5.0.0";
    version = "5.0.0";
    bin = false;
    src = fetchurl {
      url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-5.0.0.tgz";
      name = "yargs-parser-5.0.0.tgz";
      sha1 = "275ecf0d7ffe05c77e64e7c86e4cd94bf0e1228a";
    };
    deps = {
      "camelcase-3.0.0" = self.by-version."camelcase"."3.0.0";
    };
    optionalDependencies = {
    };
    peerDependencies = [];
    os = [ ];
    cpu = [ ];
  };
}

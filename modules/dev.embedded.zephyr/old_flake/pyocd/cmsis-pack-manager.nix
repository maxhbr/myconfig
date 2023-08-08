{ lib, buildPythonPackage, fetchPypi, setuptools_scm, setuptools-scm-git-archive
, buildRustCrate, appdirs, milksnake, pyyaml, pytest-runner, cargo }:
let
  time_0_2_26_ =
    { dependencies ? [ ], buildDependencies ? [ ], features ? [ ] }:
    buildRustCrate {
      crateName = "time";
      version = "0.2.26";
      authors = [ ];
      src = ./.;
      inherit dependencies buildDependencies features;
    };

in buildPythonPackage rec {
  pname = "cmsis-pack-manager";
  version = "0.2.10";

  nativeBuildInputs = [ cargo ];

  src = fetchPypi {
    inherit pname version;
    sha256 = "O1PhpzyHlPFfY08s56zSAtOs0A33wgPkWhM4bJrhrpY=";
  };

  patchPhase = ''
    cargo --version
    sed '/milksnake_tasks/d' ./setup.py
  '';

  propagatedBuildInputs = [
    setuptools_scm
    setuptools-scm-git-archive
    appdirs
    milksnake
    pyyaml
    pytest-runner
    cargo
  ];
  meta = with lib; {
    description =
      "cmsis-pack-manager is a python module, Rust crate and command line utility for managing current device information that is stored in many CMSIS PACKs.";
    homepage = "https://github.com/pyocd/cmsis-pack-manager";
    license = "Apache-2.0";
  };
}

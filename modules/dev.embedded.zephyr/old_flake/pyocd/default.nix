{ lib, buildPythonPackage, fetchPypi, setuptools_scm, setuptools-scm-git-archive

, capstone, cmsis-pack-manager, colorama, intelhex, intervaltree, naturalsort
, prettytable, pyelftools, pylink-square, pyusb, pyyaml, six }:

buildPythonPackage rec {
  pname = "pyocd";
  version = "0.30.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = "wOy7VQUhja6ZonxEV5DP+SUIHChT6qYFHO8mcHzRu7M=";
  };

  patchPhase = ''
    # break cyclic dependency
      sed '/pyocd-pemicro/d' ./setup.py
  '';

  propagatedBuildInputs = [
    setuptools_scm
    setuptools-scm-git-archive
    capstone
    cmsis-pack-manager
    colorama
    intelhex
    intervaltree
    naturalsort
    prettytable
    pyelftools
    pylink-square
    pyusb
    pyyaml
    six
  ];

  meta = with lib; {
    description =
      "pyOCD is an open source Python package for programming and debugging Arm Cortex-M microcontrollers using multiple supported types of USB debug probes.";
    homepage = "https://github.com/pyocd/pyOCD";
    license = "Apache-2.0";
  };
}

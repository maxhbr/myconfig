{ lib, buildPythonPackage, fetchPypi, setuptools_scm, setuptools-scm-git-archive
}:

buildPythonPackage rec {
  pname = "pyocd-pemicro";
  version = "1.0.4";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1ED164olepOqDnD/nFJeqTzEbuMFqecB4TNEDdN3FBA=";
  };

  propagatedBuildInputs = [ setuptools_scm setuptools-scm-git-archive ];
  meta = with lib; {
    description =
      "The simple PyOCD debug probe plugin for PEMicro debug probes - Multilink/FX, Cyclone/FX.";
    homepage = "https://github.com/pyocd/pyocd-pemicro";
    license = "BSD-3-Clause";
  };
}

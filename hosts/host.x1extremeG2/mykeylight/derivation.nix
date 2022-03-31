{ lib, python3Packages }:
with python3Packages;
buildPythonApplication {
  pname = "mykeylight";
  version = "1.0";

  propagatedBuildInputs = [ elgato ];

  src = ./.;
}

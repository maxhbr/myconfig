# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  buildPythonApplication,
  fetchFromGitHub,
  pyxdg,
  pass,
}:

buildPythonApplication rec {
  pname = "pass-git-helper";
  version = "1.0.0";

  src = fetchFromGitHub {
    sha256 = "0nn475pqhywirdprla9ihyf7pz4pv5pfc5rvc09q602fv51zc6qs";
    rev = "561c464f896edebff6d9cf3722dcb299f300ab99";
    owner = "languitar";
    repo = pname;
  };

  patchPhase = ''
    old=pass
    new=${pass}/bin/pass
    sed -i -e "s%'$old', 'show'%'$new', 'show', '-p'%g" passgithelper.py
  '';

  propagatedBuildInputs = [ pyxdg ];

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/languitar/pass-git-helper";
    description = "A git credential helper interfacing with pass, the standard unix password manager.";
    license = licenses.lgpl3;
  };
}

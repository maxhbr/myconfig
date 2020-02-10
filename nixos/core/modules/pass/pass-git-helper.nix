# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ stdenv
, buildPythonApplication
, fetchFromGitHub
, pyxdg
, pass
}:

buildPythonApplication rec {
  pname = "pass-git-helper";
  version = "1.0.0";

  src = fetchFromGitHub {
    sha256 = "1rvvwsq2yrf9lgcms5g15clah5grmz1wpf14f9wp2d3xh3df60ma";
    rev = "81fca91e4867c0d6e461aabe88e469ed9bce7034";
    repo = pname;
    owner = "languitar";
  };

  patchPhase = ''
old=pass
new=${pass}/bin/pass
sed -i -e "s%'$old', 'show'%'$new', 'show', '-p'%g" passgithelper.py
  '';

  propagatedBuildInputs = [pyxdg];

  doCheck = false;

  meta = with stdenv.lib; {
    homepage = https://github.com/languitar/pass-git-helper;
    description = "A git credential helper interfacing with pass, the standard unix password manager.";
    license = licenses.lgpl3;
  };
}
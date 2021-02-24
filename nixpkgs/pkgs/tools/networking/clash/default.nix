{ lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  pname = "clash";
  version = "1.4.0";

  src = fetchFromGitHub {
    owner = "Dreamacro";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-yTj3kXG7xB1+PhaiGgQR4bUcKkdk5eiF4bGXmxuMMsg=";
  };

  vendorSha256 = "sha256-HqlHUVWwvO15nitpwIh/u0GfF8wqJqkviyxOp7QHYz8=";

  doCheck = false;

  buildFlagsArray = [
    "-ldflags="
    "-X github.com/Dreamacro/clash/constant.Version=${version}"
  ];

  meta = with lib; {
    description = "A rule-based tunnel in Go";
    homepage = "https://github.com/Dreamacro/clash";
    license = licenses.gpl3;
    maintainers = with maintainers; [ contrun Br1ght0ne ];
  };
}

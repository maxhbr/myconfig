{ stdenv, lib, fetchFromGitHub, rustPlatform, Foundation, installShellFiles }:

rustPlatform.buildRustPackage rec {
  pname = "topgrade";
  version = "6.6.0";

  src = fetchFromGitHub {
    owner = "r-darwish";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-YMg5HWDvBsYJZCxYrQuQqU4xLY8DORKYkK319pryA5I=";
  };

  cargoSha256 = "sha256-G6ToQzPxuKpe1YQ4nLDJLjb3qx8D3VpuigXfdf7RHCQ=";

  buildInputs = lib.optional stdenv.isDarwin Foundation;

  nativeBuildInputs = [ installShellFiles ];

  postInstall = ''
    installManPage topgrade.8
  '';

  meta = with lib; {
    description = "Upgrade all the things";
    homepage = "https://github.com/r-darwish/topgrade";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ Br1ght0ne hugoreeves SuperSandro2000 ];
  };
}

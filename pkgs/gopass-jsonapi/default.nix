{ lib
, stdenv
, makeWrapper
, buildGoModule
, fetchFromGitHub
, installShellFiles
, gopass
}:

buildGoModule rec {
  pname = "gopass-jsonapi";
  version = "1.11.1";

  nativeBuildInputs = [ installShellFiles makeWrapper ];

  src = fetchFromGitHub {
    owner = "gopasspw";
    repo = pname;
    rev = "v${version}";
    sha256 = "03xhza7n92xg12z83as9qdvvc0yx1qy6q0c7i4njvng594f9a8x2";
  };

  vendorSha256 = "0d4fyppsdfzvmjb0qvpnfnw0vl6z256bly7hfb0whk6rldks60wr";

  subPackages = [ "." ];

  doCheck = false;

  buildFlagsArray = [ "-ldflags=-s -w -X main.version=${version} -X main.commit=${src.rev}" ];

  wrapperPath = lib.makeBinPath [gopass];

  postFixup = ''
    wrapProgram $out/bin/gopass-jsonapi --prefix PATH : "${wrapperPath}"
  '';

  meta = with lib; {
    description = "gopass-jsonapi enables communication with gopass via JSON messages.";
    homepage = "https://www.gopass.pw/";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.unix;
  };
}

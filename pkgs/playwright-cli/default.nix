# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeWrapper,
  playwright-driver,
}:

buildNpmPackage (finalAttrs: {
  pname = "playwright-cli";
  version = "0.1.17";

  src = fetchFromGitHub {
    owner = "microsoft";
    repo = "playwright-cli";
    rev = "v${finalAttrs.version}";
    hash = "sha256-tc/2Qck3mm6BqWTu2lvvfsM0/BHO/Z0ZvCdFZ7QQqKI=";
  };

  npmDepsHash = "sha256-u44jWprmr3RdzB3aDL3K0ShT5lLxr175z3C8pN43YFA=";

  dontNpmBuild = true;

  # playwright-cli imports playwright/lib/cli/client/program, which current
  # nixpkgs playwright-test does not export, so keep the vendored Playwright
  # until nixpkgs Playwright is updated to a compatible version.
  nativeBuildInputs = [ makeWrapper ];

  postFixup = ''
    wrapProgram $out/bin/playwright-cli \
      --set-default PLAYWRIGHT_BROWSERS_PATH ${playwright-driver.browsers}
  '';

  meta = {
    description = "Playwright CLI for browser automation";
    homepage = "https://github.com/microsoft/playwright-cli";
    changelog = "https://github.com/microsoft/playwright-cli/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ imalison ];
    mainProgram = "playwright-cli";
  };
})

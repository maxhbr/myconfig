# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# playwright-cli (https://github.com/microsoft/playwright-cli) — Playwright's
# CLI front end for coding agents. Local packaging of the same package as
# https://github.com/NixOS/nixpkgs/pull/490230, tracking the current upstream
# release and the review feedback on that PR (finalAttrs, `tag`, a compiled
# wrapper, nixpkgs' Playwright instead of the vendored alpha, an install
# check that asserts something).
{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  makeBinaryWrapper,
  versionCheckHook,
  # nixpkgs' `playwright-core`. The CLI's only runtime import is
  # `playwright-core/lib/{coreBundle,tools/cli-client/program}`, and the browser
  # revisions must come from the same Playwright version.
  playwright-driver,
  # The browser revisions the CLI launches, i.e. what playwright-core resolves
  # below `PLAYWRIGHT_BROWSERS_PATH`. Defaults to the full set from
  # `playwright-driver`; `null` pins no path at all, which is what a caller wants
  # when it launches a browser of its own through
  # `PLAYWRIGHT_MCP_EXECUTABLE_PATH` (that variable makes playwright-core ignore
  # the browsers path), or when it ships a browser set next to the CLI's
  # closure. A set narrows to one engine via
  # `playwright-driver.passthru.browsers-chromium` / `.selectBrowsers`.
  playwrightBrowsers ? playwright-driver.passthru.browsers,
}:

buildNpmPackage (finalAttrs: {
  pname = "playwright-cli";
  version = "0.1.22";

  src = fetchFromGitHub {
    owner = "microsoft";
    repo = "playwright-cli";
    tag = "v${finalAttrs.version}";
    hash = "sha256-80xzHvf7BHGvoKvMdkGeNUsUrpZrpw5eryuQM8NKT/E=";
  };

  npmDepsHash = "sha256-mGD7a/v1cx/xPGZo8nN3WA40mYGgF/KzMKiGbvUeX4E=";

  dontNpmBuild = true;

  nativeBuildInputs = [ makeBinaryWrapper ];

  # The npm package vendors `playwright`/`playwright-core` pinned to an upstream
  # alpha that matches neither `playwright-driver` nor its browser revisions.
  # Replace them with nixpkgs' `playwright-core`, the way the `playwright-mcp`
  # recipe does, so CLI, browser registry and browser revisions come from one
  # Playwright version. The `playwright` umbrella package is never required and
  # is not linked: it only re-exports `playwright-core` and would drag the full
  # browser closure into the CLI's own runtime dependencies.
  postInstall = ''
    pkg_dir="$out/lib/node_modules/@playwright/cli"
    rm -rf "$pkg_dir/node_modules/playwright" "$pkg_dir/node_modules/playwright-core"
    ln -s ${playwright-driver} "$pkg_dir/node_modules/playwright-core"
  '';

  # `--set-default` on the browsers path: a caller that ships its own browser set
  # (a sandbox tier, a devshell) overrides it by exporting the variable. Pinned
  # only when `playwrightBrowsers` names a set — an empty or absent set would
  # otherwise point playwright-core at a directory without revisions. The update
  # notifier is silenced: it queries the npm registry, which reports a version
  # the Nix packaging deliberately does not follow.
  postFixup = ''
    wrapProgram $out/bin/playwright-cli \
      ${
        lib.optionalString (
          playwrightBrowsers != null
        ) "--set-default PLAYWRIGHT_BROWSERS_PATH ${playwrightBrowsers} \\"
      } \
      --set-default NO_UPDATE_NOTIFIER 1
  '';

  doInstallCheck = true;
  nativeInstallCheckInputs = [ versionCheckHook ];
  # Unlike `playwright-mcp`, the CLI reports its OWN version: it passes
  # `embedderVersion: packageJson.version` to the shared CLI program, so
  # `--version` prints `finalAttrs.version` and the hook can assert it.
  versionCheckProgramArg = "--version";

  passthru = {
    # Package and Playwright versions are tightly coupled.
    skipBulkUpdate = true;
    # The packaged Agent Skills content, i.e. the skill the CLI itself considers
    # current (`skillCheck.js` compares an installed copy against it): `SKILL.md`
    # plus `references/`. Consumed by `myconfig.ai.dev.skills.playwright-cli`.
    skillDir = "${playwright-driver}/lib/tools/skills/playwright-cli";
  };

  meta = {
    description = "Playwright CLI for browser automation";
    homepage = "https://github.com/microsoft/playwright-cli";
    changelog = "https://github.com/microsoft/playwright-cli/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ imalison ];
    mainProgram = "playwright-cli";
  };
})

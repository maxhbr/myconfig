# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# The browser of `myconfig.ai.dev.mysbx.browser`, wrapped so every
# invocation carries `--no-sandbox`.
#
# A chrome-family browser sandboxes its own renderers with a user
# namespace, and inside mysbx it cannot: bubblewrap already runs the
# payload in a user namespace with `no_new_privs`, which forecloses both
# the SUID helper and the nested namespace the zygote needs. The browser
# then dies at startup instead of falling back. Passing `--no-sandbox`
# gives up a layer the payload never had, INSIDE a sandbox that is the
# actual boundary — the renderer stays confined by bubblewrap (or by
# gVisor on the podman backend) exactly like every other process of the
# run.
#
# The flag is baked in rather than left to the caller because every
# caller is a machine: `agent-browser` spawns the executable named by
# `AGENT_BROWSER_EXECUTABLE_PATH` (or found on `PATH`) with its own
# argument list, and there is no hook to add a flag to it.
#
# `symlinkJoin` keeps the whole package — the main executable and
# whatever else it ships — and keeps the executable's NAME, which the
# PATH probes of chrome-family clients look for (`chromium`,
# `google-chrome`, ...). Arguments pass through untouched: `--add-flags`
# prepends, so a caller's flags still come last and win.
#
# The package's OTHER names for the same executable are re-pointed at
# the wrapper (`chromium-browser` next to `chromium`): a probe walks a
# list of names and takes the first hit, so an alias left pointing at
# the bare binary would silently skip the flag. Entries that resolve
# somewhere else are left alone — they are different programs.
{
  lib,
  symlinkJoin,
  makeWrapper,
  browser,
}:
let
  exe = baseNameOf (lib.getExe browser);
in
symlinkJoin {
  name = "${lib.getName browser}-no-sandbox";
  paths = [ browser ];
  nativeBuildInputs = [ makeWrapper ];
  postBuild = ''
    target="$(readlink -f "$out/bin/${exe}")"
    wrapProgram "$out/bin/${exe}" --add-flags "--no-sandbox"
    for alias in "$out"/bin/*; do
      name="$(basename "$alias")"
      case "$name" in
        ${exe} | .*) continue ;;
      esac
      if [ "$(readlink -f "$alias")" = "$target" ]; then
        ln -sf "${exe}" "$alias"
      fi
    done
  '';
  meta = (browser.meta or { }) // {
    mainProgram = exe;
  };
}

# Drop the Kolibri-1.1 llama.cpp fork pin once the PR lands in mainline

## What to do

Remove `hosts/host.thing/nixpkgs.overlays.llama-cpp-kolibri.nix` (file,
import in `hosts/host.thing/default.nix`, and the `llama-cpp-kolibri`
handover in `hosts/host.thing/myconfig.ai.llmops.llama-cpp/default.nix`)
once Kolibri-1.1 support is in the mainline build the host pins:

1. PR ggml-org/llama.cpp#32891 ("model : add Kolibri-1.1 support") must
   be merged (pinned fork: hirschsn/llama.cpp @ `5f4d2228`).
2. The inkling pin (`nixpkgs.overlays.llama-cpp-inkling.nix`, rev
   `690b87d`) must move past the merge — then the stock
   `pkgs.llama-cpp-rocm` loads Kolibri-1.1 and the model entry can drop
   `serverPackage` (and its ROCm0-only restriction can stay: Vulkan
   needs a round-nvfp4 SPIR-V kernel, which the PR does not add).
3. Delete the `serverPackage` from
   `hosts/host.thing/myconfig.ai.llmops.llama-cpp/Kolibri-1.1.nix`
   (keep `devices = [ "ROCm0" ]`), remove the `llama-cpp-kolibri`
   argument and the filter exclusions for `Kolibri-1.1-Q8_0` in
   `default.nix` (the entries rejoin the plain amdModels maps).

## Verification

- `nix eval --raw .#nixosConfigurations.test-thing.config.system.build.toplevel.drvPath`
  evaluates; the container's llama-swap lists `Kolibri-1.1-Q8_0` with a
  cmd that execs the inkling/mainline `llama-cpp-rocm`.
- `llama-server_ROCm0_Kolibri-1.1-Q8_0` reaches "model loaded" on the
  gfx1151 iGPU (not just a known arch name — see the d74bae2cd2 lesson).

## Affected code

- `hosts/host.thing/nixpkgs.overlays.llama-cpp-kolibri.nix` (the pin)
- `hosts/host.thing/default.nix` (import)
- `hosts/host.thing/myconfig.ai.llmops.llama-cpp/default.nix`
  (`llama-cpp-kolibri` argument, two `Kolibri-1.1-Q8_0` exclusions from
  the Vulkan0/Vulkan1 device-forcing maps)
- `hosts/host.thing/myconfig.ai.llmops.llama-cpp/Kolibri-1.1.nix`
  (`serverPackage`, `devices`)
- see also `doc/TODOs/serve-kolibri-1.1-llama-cpp-pr-32891.md` (the
  original re-enable checklist, kept for the runtime-validation steps)

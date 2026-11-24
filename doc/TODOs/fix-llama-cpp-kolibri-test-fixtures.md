# Re-enable the Kolibri-1.1 test fixtures in the llama.cpp fork pin

## What to do

Remove the `disabledTests` addition (the `test-generate-models` and
`test-llama-context` entries) from
`hosts/host.thing/nixpkgs.overlays.llama-cpp-kolibri.nix` once the pinned
llama.cpp fork generates dummy models that its own loader accepts.

## Preconditions

- Upstream PR ggml-org/llama.cpp#32891 ("model : add Kolibri-1.1
  support", pinned here at head `5f4d222898ddb12cf581edf1b7e841d80e0b2e17`)
  must update `tests/test-llama-archs.cpp` so the generated kolibri
  dummy models carry the new hparams the loader reads, or the loader
  must tolerate their absence.

## Affected code

- `hosts/host.thing/nixpkgs.overlays.llama-cpp-kolibri.nix` — the
  `disabledTests` override on the fork derivation.
- Failure (reproduced with a plain CPU build of the fork):

      test-llama-archs -o <dir>  →
      E llama_model_load: error loading model hyperparameters:
        key not found in model: kolibri1.attention.indexer.candidate_block_size

  The official `Aleph-Alpha/Kolibri-1.1-GGUF` files are unaffected
  (the PR's `conversion/kolibri1.py` writes the key from the HF
  config's `index_compression`); only the test fixtures are broken.
- Introduced with the fork pin itself (commit that re-enabled
  Kolibri-1.1-Q8_0 on ROCm0).

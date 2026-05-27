# Model Directory Reorganization Plan

## Background

The `pull-models` script has been updated to keep the HuggingFace org prefix in the local path:
- **Old behavior**: `org/repo/file.gguf` → `$dir/repo/file.gguf`
- **New behavior**: `org/repo/file.gguf` → `$dir/org-repo/file.gguf`

All existing model files in `/home/mhuber/models/` need to be moved to match the new naming scheme.

## Move Commands

Execute these commands to rename the directories:

```bash
# Qwen3.6-27B (unsloth)
mv /home/mhuber/models/Qwen3.6-27B-GGUF /home/mhuber/models/unsloth-Qwen3.6-27B-GGUF

# Qwen3.6-35B-A3B (unsloth)
mv /home/mhuber/models/Qwen3.6-35B-A3B-GGUF /home/mhuber/models/unsloth-Qwen3.6-35B-A3B-GGUF

# gemma-4-31B-it (unsloth)
mv /home/mhuber/models/gemma-4-31B-it-GGUF /home/mhuber/models/unsloth-gemma-4-31B-it-GGUF

# gemma-4-26B-A4B-it (unsloth)
mv /home/mhuber/models/gemma-4-26B-A4B-it-GGUF /home/mhuber/models/unsloth-gemma-4-26B-A4B-it-GGUF

# MiniMax-M2.7 (unsloth)
mv /home/mhuber/models/MiniMax-M2.7-GGUF /home/mhuber/models/unsloth-MiniMax-M2.7-GGUF

# Nemotron-3-Super (AesSedai)
mv /home/mhuber/models/Nemotron-3-Super-120B-A12B-GGUF /home/mhuber/models/AesSedai-Nemotron-3-Super-120B-A12B-GGUF

# Qwen3.5-9B (unsloth)
mv /home/mhuber/models/Qwen3.5-9B-GGUF /home/mhuber/models/unsloth-Qwen3.5-9B-GGUF

# Qwen3.5-122B-A10B (unsloth)
mv /home/mhuber/models/Qwen3.5-122B-A10B-GGUF /home/mhuber/models/unsloth-Qwen3.5-122B-A10B-GGUF
```

## Summary Table

| Model | Old Directory | New Directory | hf_spec |
|-------|---------------|---------------|----------|
| Qwen3.6-27B | `Qwen3.6-27B-GGUF/` | `unsloth-Qwen3.6-27B-GGUF/` | `unsloth/Qwen3.6-27B-GGUF/...` |
| Qwen3.6-35B-A3B | `Qwen3.6-35B-A3B-GGUF/` | `unsloth-Qwen3.6-35B-A3B-GGUF/` | `unsloth/Qwen3.6-35B-A3B-GGUF/...` |
| gemma-4-31B-it | `gemma-4-31B-it-GGUF/` | `unsloth-gemma-4-31B-it-GGUF/` | `unsloth/gemma-4-31B-it-GGUF/...` |
| gemma-4-26B-A4B-it | `gemma-4-26B-A4B-it-GGUF/` | `unsloth-gemma-4-26B-A4B-it-GGUF/` | `unsloth/gemma-4-26B-A4B-it-GGUF/...` |
| MiniMax-M2.7 | `MiniMax-M2.7-GGUF/` | `unsloth-MiniMax-M2.7-GGUF/` | `unsloth/MiniMax-M2.7-GGUF/...` |
| Nemotron-3-Super | `Nemotron-3-Super-120B-A12B-GGUF/` | `AesSedai-Nemotron-3-Super-120B-A12B-GGUF/` | `AesSedai/NVIDIA-Nemotron-3-Super-120B-A12B-GGUF/...` |
| Qwen3.5-9B | `Qwen3.5-9B-GGUF/` | `unsloth-Qwen3.5-9B-GGUF/` | `unsloth/Qwen3.5-9B-GGUF/...` |
| Qwen3.5-122B-A10B | `Qwen3.5-122B-A10B-GGUF/` | `unsloth-Qwen3.5-122B-A10B-GGUF/` | `unsloth/Qwen3.5-122B-A10B-GGUF/...` |

## Nix Config Updates Required

After moving the files, the `path =` values in these files need to be updated:

- `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.6-27B.nix`
- `hosts/host.thing/myconfig.ai.llama-cpp/Qwen3.6-35B-A3B.nix`
- `hosts/host.thing/myconfig.ai.llama-cpp/gemma4.nix`
- `hosts/host.thing/myconfig.ai.llama-cpp/MiniMax-M2.7-GGUF.nix`
- `hosts/host.thing/myconfig.ai.llama-cpp/Nemotron-3-Super.nix`
- `hosts/host.thing/myconfig.ai.llama-cpp/default.nix`
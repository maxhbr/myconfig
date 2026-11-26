# host.thing

AI workstation: Framework Laptop 16 with AMD Ryzen AI Max+ 395
(Radeon 8060S iGPU, 128 GiB unified memory) plus a Thunderbolt
NVIDIA RTX 5090 eGPU.

Per-model documentation (paths, quantisations, context sizes,
aliases, variants) is **generated** at activation time:

- `/run/myconfig/docs/models.md` — every declared GGUF model
  (rendered by `modules/myconfig.ai.llmops/myconfig.ai.llmops.llama-cpp/docs.nix`)
- `/run/myconfig/localModels.json` — the provider/model registry
  published for litellm / pi / opencode

This file only documents what those generated docs cannot: the
serving topology — which server runs where, on which GPU, at which
port, and how it is started.

## GPUs

| Device string | Hardware | Notes |
|---|---|---|
| `CUDA0` / `Vulkan0` (nvidia) | RTX 5090 eGPU (32 GB, Thunderbolt) | `hardware.RTX5090.nix`; CUDA via nvidia-container-toolkit CDI for containers |
| `ROCm0` / `Vulkan0` (amd) | Radeon 8060S iGPU (gfx1151, 124 GiB GTT/TTM) | `hardware.Radeon8060S.nix`; `HSA_OVERRIDE_GFX_VERSION=11.5.1`; compositor pinned to the iGPU |

`myconfig.hardware.gpu.variant = ["nvidia" "amd"]` — llama-cpp
device guards (`lib/devices.nix`) keep only entries whose backend
variant the host actually has.

## How models are served

Three independent serving families; none of them conflict at runtime
because each owns its GPU + port.

| Backend | Port | GPU / device | Runs as | Defined in |
|---|---|---|---|---|
| llama-server router (`rtx5090`) | 33656 | CUDA0 (RTX 5090) | host systemd (`services.llama-cpp`) | `myconfig.ai.llmops.llama-cpp/default.nix` (`rtx-llama-cpp-config`, `serviceVariant = "llama-server"`) |
| llama-swap (`gfx1151`) | 33657 | Vulkan0/ROCm0 (iGPU) | **NixOS container** `llama-cpp-33657` | same file (`gfx-llama-cpp-config`, `serviceVariant = "llama-swap"`) |
| container launchers (vllm / ninfer / sglang) | 22545 (CUDA), 22549 (ROCm) | see below | rootless podman, hand-launched | `myconfig.ai.vllm/`, `myconfig.ai.ninfer/`, `myconfig.ai.sglang/` |

### 1. Host llama-server router — port 33656 (`rtx5090`)

A single `llama-server` instance bound to CUDA0 with an
INI-preset listing every rtx GGUF model (63 published names
including aliases/variants). One model resident at a time
(`models-max = 1`); the OpenAI `model` field selects (and swaps)
the preset section. Ad-hoc per-device wrappers
(`llama-server_CUDA0_<Model>` etc.) are also installed via
home-manager (`router.enable`), independent of the service.

Published to litellm / downstream tools as provider **`rtx5090`**.

### 2. Container llama-swap — port 33657 (`gfx1151`)

The `llama-cpp-33657` NixOS container (`privateNetwork = false`,
shares host network) runs `services.llama-swap` on the iGPU with
per-(model, device) wrapper commands — the llama-swap proxy swaps
models in/out on request and by TTL (183 published names). Device
access via bind mounts (`/dev/dri`, `/dev/kfd`,
`/run/opengl-driver`) and `allowedDevices`; models are read
read-only from `/models`.

Published as provider **`gfx1151`**. Also the scrape target of the
`llama-swap` Prometheus job
(`myconfig.observability.llama-swap-metrics.nix`) — reachable at
`https://gfx1151.thing.wg0.maxhbr.local/metrics` via Caddy.

One model is **ROCm-only inside the swap**: `Kolibri-1.1-Q8_0` is
served by a per-model fork pin (`serverPackage =
llama-cpp-kolibri`, from `nixpkgs.overlays.llama-cpp-kolibri.nix` —
PR ggml-org/llama.cpp#32891 pinned at `5f4d222`), because no
mainline build loads the Kolibri-1.1 layout. The fork has no Vulkan
`round-nvfp4` kernel, so the model gets no Vulkan device (unlike
every other swap entry); it is also excluded from the rtx INI router
(the 74.4 GiB Q8_0 weights do not fit the RTX 5090).

### 3. Container engine launchers — ports 22545 / 22549

vLLM, NInfer and SGLang run as rootless **podman** containers,
started by hand via generated wrapper binaries (no systemd unit
owns them; `environment.systemPackages` only). All CUDA variants
share port 22545 and swap the whole container in/out; the ROCm
variants (Nex-N2.5-mini, Qwen3.8-27B-FP8) share 22549 on the iGPU.

| Launcher | Port | GPU | Image |
|---|---|---|---|
| `vllm-dockerized-Qwen3.8-27B-*` (NVFP4, MTP-NVFP4, NVFP4-RTX5090) | 22545 | RTX 5090 (CDI `nvidia.com/gpu=all`) | `docker.io/vllm/vllm-openai:v0.27.1` |
| `ninfer-dockerized-Qwen3.8-27B-NVFP4` | 22545 | RTX 5090 (CDI) | locally built `ninfer:local` |
| `sglang-qwen38-27b-dflash2` | 22545 | RTX 5090 (`--gpus all`) | `docker.io/lmsysorg/sglang:dev-qwen38-27b-dflash2` |
| `vllm-dockerized-Nex-N2.5-mini-ROCm`, `-Qwen3.8-27B-FP8-ROCm` | 22549 | iGPU (`/dev/kfd`, `/dev/dri`) | `docker.io/rocm/vllm:rocm7.13.0_gfx1151_...` |

Clients reach them **directly** on the container port
(`{ port = 22545; }` in `myconfig.ai.llmops.localModels` → the
`localhost:22545` model in litellm/pi/aichat) — whichever container
is currently running answers. The litellm model aliases
(`vllm:hermes`, `vllm:opencode`, …) expect this port.

### Known-broken: `llama-swap.service` on the host

`systemctl status llama-swap.service` shows
`Loaded: bad-setting — Service has no ExecStart=`:

- `services.llama-swap.enable` is **false** on the host (the iGPU
  llama-swap lives in the container; the host GGUF backend is the
  llama-server router). The nixpkgs upstream module defines
  `ExecStart` only under `mkIf cfg.enable`.
- But `myconfig.ai.vllm/default.nix` unconditionally declares
  `systemd.services.llama-swap.{wants,after}` (podman + NVIDIA CDI
  start deps for the container launchers), which materialises a
  bare unit with only `Wants=`/`After=` — systemd refuses to load
  it. Nothing references it, so it is inert.
- Likewise inert: the `services.llama-swap.settings.models` proxy
  entries (vllm/ninfer `modelConfig`) — written for a host
  llama-swap front door on 22545 that no longer exists; the
  container launchers are addressed directly instead.

These are documented, not yet fixed.

## Other AI services

| Service | Port | Notes |
|---|---|---|
| LiteLLM proxy | 4000 | `services.litellm.nix`; aggregates localModels providers (`rtx5090`, `gfx1151`, `localhost:22545/22546`) |
| ComfyUI | 8188 | systemd **user** service (`userservice = true`), CUDA or ROCm build |
| open-webui | (disabled) | commented out in `default.nix` |

## VMs

| VM | Purpose | Defined in |
|---|---|---|
| `ai-gpu-vm` (Ubuntu 24.04) | RTX 5090 VFIO passthrough for CUDA workloads not supported on NixOS host | `rtx-vm/` (libvirt/virsh; bind/unbind via `scripts/bind-gpu-to-vfio.sh`) |
| mysbx agent sandboxes | hermes service (microvm, autostart), krun / microvm-qemu / microvm-ch dev sandboxes | `myconfig.ai.dev.mysbx` options (base config), `myconfig.dev.embedded` profile (modules/dev/dev.embedded.nix) |

## WireGuard access

`thing.wg0.maxhbr.local` / `thing.wg0`; Caddy on 443 exposes
`rtx5090` (33656), `gfx1151` (33657), `litellm` (4000), `forgejo`,
`orca`, `comfyui` — see `../shared.deployedServices.nix`.

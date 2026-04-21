# host.thing

AI workstation with AMD Radeon 8060S GPU (Framework Laptop 16).

## Services & Ports

| Service | Port | Description |
|---------|------|-------------|
| eternal-terminal | 22022 | TCP/UDP |
| vllm-rocm (container) | 8000 | OCI container with vllm |
| searxng | 28080 | Private metasearch engine (UWSGI HTTP) |
| llama-swap | 33656 | Model swap proxy (host) |
| llama-swap-33657 | 33657 | Model swap proxy (container) |
| comfyui | 8188 | ComfyUI (via `run-comfyui` script) |
| caddy (reverse proxy) | 443 | WireGuard tunnel access |
| litellm | (dynamic) | LiteLLM proxy |
| open-webui | (dynamic) | Open WebUI |

## WireGuard Access

Services are accessible via WireGuard tunnel:
- `thing.wg0.maxhbr.local`
- `thing.wg0`

Caddy routes available at port 443:
- `/litellm/*` → LiteLLM
- `/ollama/*` → Ollama
- `/open-webui/*` → Open WebUI
- `/comfyui/*` → ComfyUI
- (default) → llama-swap

## Key Features

- **AI/ML**: AMD ROCm support, llama-cpp-vulkan, vllm container
- **Model Management**: llama-swap for automatic model switching with TTL
- **Desktop**: Wayland (niri, labwc), WayVNC for remote access
- **Impermanence**: Btrfs + LUKS with tmpfs overlay

## Scripts

- `run-comfyui` - Setup and run ComfyUI with CUDA support

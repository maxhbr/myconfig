# host.thing

AI workstation with AMD Radeon 8060S GPU (Framework Laptop 16).

## Services & Ports

| Service | Port | Description |
|---------|------|-------------|
| eternal-terminal | 22022 | TCP/UDP |
| vllm-rocm (container) | 8000 | OCI container with vllm |
| llama-server | 33656 | llama-server router (host, CUDA0-only) |
| llama-cpp-33657 | 33657 | llama-server router (container, Vulkan0-only) |
| comfyui | 8188 | ComfyUI (via `run-comfyui` script) |
| caddy (reverse proxy) | 443 | WireGuard tunnel access |
| dolt sql-server | 3307 | MySQL-compatible, bd (beads) backend |
| litellm | (dynamic) | LiteLLM proxy |
| open-webui | (dynamic) | Open WebUI |

## WireGuard Access

Services are accessible via WireGuard tunnel:
- `thing.wg0.maxhbr.local`
- `thing.wg0`

Caddy routes available at port 443:
- `/litellm/*` → LiteLLM
- `/open-webui/*` → Open WebUI
- `/comfyui/*` → ComfyUI
- (default) → llama-server (port 33656)

## bd (beads) via the Dolt SQL server

`services.dolt.nix` runs a `dolt sql-server` on `thing.wg0:3307` (MySQL
wire protocol) as the shared bd backend. Point bd at it in server mode:

```bash
export BEADS_DOLT_SERVER_HOST=thing.wg0.maxhbr.local \
       BEADS_DOLT_SERVER_PORT=3307
bd dolt test
```

Setting the port also tells bd the server is externally managed, so bd
never spawns its own. To migrate an existing embedded project:

```bash
bd backup init /tmp/beads-backup && bd backup sync
bd init --server && bd backup restore --force /tmp/beads-backup
# then, on thing: cp -r .beads/dolt/<project> /var/lib/dolt/<project>
```

## Key Features

- **AI/ML**: AMD ROCm support, llama-cpp-vulkan, vllm container
- **Model Management**: llama-server router backend with INI-driven model presets (--models-max 1, swap-on-demand via the OpenAI `model` field)
- **Desktop**: Wayland (niri, labwc), WayVNC for remote access
- **Impermanence**: Btrfs + LUKS with tmpfs overlay

## Scripts

- `run-comfyui` - Setup and run ComfyUI with CUDA support

# The vLLM CUDA/ROCm variant modules are loaded together on this host.
# The llama-swap unit's start dependencies (Podman, and the NVIDIA CDI
# generator that the CUDA containers use for GPU access) are declared
# *here* — the single shared declaration site for the container-based
# engine modules on this host, including the separate NInfer engine
# module in ../myconfig.ai.ninfer/ (imported alongside on this host) —
# instead of in each variant module, so the generated systemd unit does
# not carry duplicate Wants=/After= entries.
{
  config,
  lib,
  ...
}:
{
  imports = [
    ./docker.vllm.cuda.nix
    ./docker.vllm.rocm.nix
  ];

  # Gated on services.llama-swap.enable: an unconditional wants/after
  # declaration creates a llama-swap.service with no ExecStart when the
  # service is disabled, which systemd refuses to load ("bad unit file
  # setting").
  config = lib.mkIf config.services.llama-swap.enable {
    systemd.services.llama-swap = {
      wants = [
        "podman.service"
        "podman.socket"
        # NVIDIA CDI specs (nvidia.com/gpu=*) for the CUDA container
        # variants (vLLM and NInfer).
        "nvidia-container-toolkit-cdi-generator.service"
      ];
      after = [
        "podman.service"
        "podman.socket"
        "nvidia-container-toolkit-cdi-generator.service"
      ];
    };
  };
}

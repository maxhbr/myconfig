# The vLLM CUDA/ROCm variant modules and the NInfer CUDA variant
# module are loaded together on this host. The llama-swap unit's start
# dependencies (Podman, and the NVIDIA CDI generator that the CUDA
# containers use for GPU access) are declared *here* instead of in
# each variant module, so the generated systemd unit does not carry
# duplicate Wants=/After= entries.
{
  imports = [
    ./docker.vllm.cuda.nix
    ./docker.vllm.rocm.nix
    ./docker.ninfer.cuda.nix
  ];

  config = {
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

# The CUDA and ROCm variant modules are loaded together on this host.
# The Podman start dependencies of the llama-swap unit are declared
# *here* instead of in both variant modules, so the generated systemd
# unit does not carry duplicate Wants=/After= entries. (The CUDA
# module separately adds the NVIDIA CDI generator dependency on top of
# this.)
{
  imports = [
    ./docker.vllm.cuda.nix
    ./docker.vllm.rocm.nix
  ];

  config = {
    systemd.services.llama-swap = {
      wants = [
        "podman.service"
        "podman.socket"
      ];
      after = [
        "podman.service"
        "podman.socket"
      ];
    };
  };
}

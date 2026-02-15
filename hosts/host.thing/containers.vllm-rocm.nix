{ config, pkgs, ... }:

let
  hfCacheHostPath = "/persistent/cache/vllm/huggingface-cache";
  image = "rocm/vllm:latest";
  model = "zai-org/GLM-4.7-Flash";
  hostPort = 8000;
in
{
  systemd.tmpfiles.rules = [
    "d ${hfCacheHostPath} 0755 root root -"
  ];

  #### Container
  virtualisation.oci-containers.containers.vllm-rocm = {
    image = image;
    autoStart = true;

    ports = [
      "${toString hostPort}:8000"
    ];

    volumes = [
      "${hfCacheHostPath}:/root/.cache/huggingface"
    ];

    environment = {
      HF_HOME = "/root/.cache/huggingface";
      HSA_OVERRIDE_GFX_VERSION = "11.5.1";
      ROCM_VISIBLE_DEVICES = "0";
    };

    extraOptions = [
      "--device=/dev/kfd"
      "--device=/dev/dri"
      "--group-add=render"
      "--group-add=video"

      # If you run into permission/SELinux/apparmor-ish denials, uncomment ONE of these:
      # "--security-opt=label=disable"
      # "--security-opt=seccomp=unconfined"
    ];

    cmd = [
      "vllm"
      "serve"
      model
      "--host"
      "0.0.0.0"
      "--port"
      "8000"
      "--dtype"
      "auto"
      "--gpu-memory-utilization"
      "0.90"
    ];
  };
}

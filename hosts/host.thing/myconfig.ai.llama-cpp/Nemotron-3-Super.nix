let
  modelsPullDir = "/home/mhuber/models";
in
{
  amdModels = [
    {
      name = "NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M";
      path = "/models/AesSedai-NVIDIA-Nemotron-3-Super-120B-A12B-GGUF/Q5_K_M/NVIDIA-Nemotron-3-Super-120B-A12B-BF16-Q5_K_M-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "AesSedai/NVIDIA-Nemotron-3-Super-120B-A12B-GGUF/Q5_K_M" ];
      };
      ttl = 1500;
    }
  ];
}

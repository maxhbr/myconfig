let
  modelsPullDir = "/home/mhuber/models";
in
{
  amdModels = [
    {
      name = "Qwen3-235B-A22B-Instruct-Q2_K_L";
      path = "/models/unsloth-Qwen3-235B-A22B-Instruct-2507-GGUF/Q2_K_L/Qwen3-235B-A22B-Instruct-2507-Q2_K_L-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/Qwen3-235B-A22B-Instruct-2507-GGUF/Q2_K_L"
        ];
      };
      ttl = 3600;
    }
  ];
}
